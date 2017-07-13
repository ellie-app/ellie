import json
import multiprocessing
import os
import shutil
import sys
import tempfile
import zipfile
from datetime import datetime
from typing import (Any, Dict, Iterator, List, NamedTuple, Optional, Set,
                    SupportsInt, Tuple, TypeVar)

import boto3
import glob2
import requests
from joblib import Parallel, delayed

from . import storage
from .classes import Constraint, PackageInfo, Version

BUCKET_NAME = os.environ['AWS_S3_BUCKET']

s3 = boto3.resource('s3')
bucket = s3.Bucket(BUCKET_NAME)

T = TypeVar('T')


def cat_optionals(data: Iterator[Optional[T]]) -> List[T]:
    out = []
    for x in data:
        if x is not None:
            out.append(x)
    return out


def glob_all(paths: List[str]) -> List[str]:
    output: List[str] = []
    for path in paths:
        output = output + glob2.glob(path)
    return output


def download_packages() -> Any:
    body = requests.get("http://package.elm-lang.org/all-packages")
    data = body.json()
    body.close()
    return data


def organize_packages(data: Any) -> List[PackageInfo]:
    output = []
    for entry in data:
        username = entry['name'].split('/')[0]
        package = entry['name'].split('/')[1]
        for version in entry['versions']:
            v = Version.from_string(version)
            if v is not None:
                output.append(PackageInfo(username, package, v))
    return output


def make_temp_directory() -> str:
    return tempfile.mkdtemp(prefix='ellie-package-temp-')


def download_package_zip(base_dir: str, package: PackageInfo) -> None:
    url = 'http://github.com/' + package.username + '/' + package.package + '/archive/' + str(
        package.version) + '.zip'
    zip_path = os.path.join(base_dir, 'temp.zip')
    try:
        r = requests.get(url)
        with open(zip_path, "wb") as local_file:
            for chunk in r.iter_content(chunk_size=128):
                local_file.write(chunk)
    except Exception as e:
        print("HTTP Error:", e)


def unzip_and_delete(base_dir: str) -> None:
    zip_path = os.path.join(base_dir, 'temp.zip')
    zip_ref = zipfile.ZipFile(zip_path, 'r')
    zip_ref.extractall(base_dir)
    zip_ref.close()
    os.remove(zip_path)


def read_package_json(base_dir: str, package: PackageInfo) -> Any:
    package_json_path = os.path.join(
        base_dir, package.package + '-' + str(package.version),
        'elm-package.json')
    return json.loads(open(package_json_path, 'r').read())


def read_source_files(base_dir: str, package: PackageInfo,
                      package_json: Any) -> Any:
    package_dir = os.path.join(base_dir,
                               package.package + '-' + str(package.version))
    nested_elm = [
        os.path.join(package_dir, a, '**/*.elm')
        for a in package_json['source-directories']
    ]
    nested_js = [
        os.path.join(package_dir, a, '**/*.js')
        for a in package_json['source-directories']
    ]
    filenames = glob_all(nested_elm + nested_js)
    output = {}
    for filename in filenames:
        output[filename.replace(package_dir + '/', '')] = open(filename,
                                                               'r').read()
    return output


def get_last_updated() -> int:
    body = s3.Object(BUCKET_NAME,
                     'package-artifacts/last-updated').get()['Body']
    data = body.read()
    time = json.loads(data)
    body.close()
    return time


def get_current_time() -> int:
    epoch = datetime.utcfromtimestamp(0)
    dt = datetime.utcnow()
    return int((dt - epoch).total_seconds() * 1000.0)


min_required_version = Version(0, 18 0)


def process_package(package: PackageInfo) -> Tuple[bool, PackageInfo]:
    try:
        base_dir = make_temp_directory()
        download_package_zip(base_dir, package)
        unzip_and_delete(base_dir)
        package_json = read_package_json(base_dir, package)
        constraint = Constraint.from_string(package_json['elm-version'])
        package.set_elm_constraint(constraint)
        if not constraint.is_satisfied(min_required_version):
            return (True, package)

        source_files = read_source_files(base_dir, package, package_json)
        bucket.put_object(
            Key=package.s3_package_key(),
            ACL='public-read',
            Body=json.dumps(package_json).encode('utf-8'),
            ContentType='application/json')
        bucket.put_object(
            Key=package.s3_source_key(),
            ACL='public-read',
            Body=json.dumps(source_files).encode('utf-8'),
            ContentType='application/json')
        shutil.rmtree(base_dir)
        return (True, package)
    except:
        print(package)
        print(sys.exc_info())
        return (False, package)


def upload_searchable_packages(packages: List[PackageInfo]) -> None:
    bucket.put_object(
        Key='package-artifacts/searchable.json',
        ACL='public-read',
        Body=json.dumps([x.to_json() for x in packages]).encode('utf-8'),
        ContentType='application/json')


def download_searchable_packages() -> Set[PackageInfo]:
    try:
        body = s3.Object(BUCKET_NAME,
                         'package-artifacts/searchable.json').get()['Body']
        data = body.read()
        packages = set(
            cat_optionals(PackageInfo.from_json(x) for x in json.loads(data)))
        body.close()
        return packages
    except:
        return set()


def run() -> None:
    data = download_packages()
    num_cores = multiprocessing.cpu_count()
    packages = organize_packages(data)
    searchable = download_searchable_packages()
    filtered_packages = [p for p in packages if p not in searchable]
    package_groups = [
        filtered_packages[x:x + num_cores]
        for x in range(0, len(filtered_packages), num_cores)
    ]
    counter = 0.0
    total = float(len(filtered_packages))
    failed = []
    for package_group in package_groups:
        results = Parallel(n_jobs=num_cores)(delayed(process_package)(i)
                                             for i in package_group)
        counter += num_cores
        for (succeeded, package) in results:
            if succeeded:
                searchable.add(package)
            else:
                failed.append(package)

        print(str(counter / total * 100) + '%')

    upload_searchable_packages(list(searchable))


if __name__ == "__main__":
    run()
