import base64
import json
import multiprocessing
import os
import shutil
import subprocess
import sys
import tempfile
import traceback
import zipfile
from datetime import datetime
from typing import (Any, Dict, Iterator, List, NamedTuple, Optional, Set,
                    SupportsInt, Tuple, TypeVar)

import boto3
import glob2
import requests
from joblib import Parallel, delayed

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
    json_data = None
    with open(package_json_path, 'r') as file_data:
        json_data = json.loads(file_data.read())
    return json_data


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
        with open(filename, 'r') as file_data:
            output[filename.replace(package_dir + '/', '')] = file_data.read()
    return output


def read_artifacts(base_dir: str, package: PackageInfo) -> Any:
    artifacts_base = os.path.join(base_dir,
                                  package.package + '-' + str(package.version),
                                  'elm-stuff/build-artifacts/0.18.0/',
                                  package.username,
                                  package.package,
                                  str(package.version))
    artifacts = [
        os.path.join(artifacts_base, '*.elmo'),
        os.path.join(artifacts_base, '*.elmi')
    ]

    filenames = glob_all(artifacts)
    output = {}
    for filename in filenames:
        key = filename.replace(artifacts_base + '/', '')
        with open(filename, 'rb') as file_data:
            if filename.endswith('elmi'):
                output[key] = base64.b64encode(
                    file_data.read()).decode('utf-8')
            else:
                output[key] = file_data.read().decode('utf-8')

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


min_required_version = Version(0, 18, 0)


def needs_prebuild(package: PackageInfo) -> bool:
    return package.username == 'elm-lang' or (package.username == 'rtfeldman' and package.package == 'elm-css')


def process_package(package: PackageInfo) -> Tuple[bool, PackageInfo]:
    try:
        base_dir = make_temp_directory()
        download_package_zip(base_dir, package)
        unzip_and_delete(base_dir)
        package_json = read_package_json(base_dir, package)
        constraint = Constraint.from_string(package_json['elm-version'])
        if constraint is None:
            shutil.rmtree(base_dir)
            return (False, package)

        if not constraint.is_satisfied(min_required_version):
            shutil.rmtree(base_dir)
            return (False, package)

        package.set_elm_constraint(constraint)

        if needs_prebuild(package):
            elm_path = os.path.realpath(
                os.path.dirname(os.path.realpath(__file__)) +
                '/../node_modules/elm/Elm-Platform/0.18.0/.cabal-sandbox/bin/elm-make')

            package_dir = os.path.join(
                base_dir, package.package + '-' + str(package.version))

            process_output = subprocess.run(
                [elm_path, '--yes'],
                cwd=package_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)

            if process_output.returncode != 0:
                stderr_as_str = process_output.stderr.decode('utf-8')
                raise Exception(stderr_as_str)

            artifacts = read_artifacts(base_dir, package)
            bucket.put_object(
                Key=package.s3_artifacts_key(Version(0, 18, 0)),
                ACL='public-read',
                Body=json.dumps(artifacts).encode('utf-8'),
                ContentType='application/json')

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
        shutil.rmtree(base_dir)
        print(package)
        print(sys.exc_info())
        return (False, package)


def upload_searchable_packages(packages: List[PackageInfo]) -> None:
    bucket.put_object(
        Key='package-artifacts/searchable.json',
        ACL='public-read',
        Body=json.dumps([x.to_json() for x in packages]).encode('utf-8'),
        ContentType='application/json')


def upload_failed_packages(packages: List[PackageInfo]) -> None:
    bucket.put_object(
        Key='package-artifacts/known_failures.json',
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


def download_known_failures() -> Set[PackageInfo]:
    try:
        body = s3.Object(BUCKET_NAME,
                         'package-artifacts/known_failures.json').get()['Body']
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

    print('sync_packages: downloading package data')

    packages = organize_packages(data)
    searchable = download_searchable_packages()
    known_failures = download_known_failures()
    filtered_packages = [
        p for p in packages if p not in searchable and p not in known_failures]
    package_groups = [
        filtered_packages[x:x + num_cores]
        for x in range(0, len(filtered_packages), num_cores)
    ]
    counter = 0
    total = len(filtered_packages)
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

        print('sync_packages: ' + str((counter * 100) // total) + '%')

    upload_searchable_packages(list(searchable))
    upload_failed_packages(failed + list(known_failures))
    print('sync_packages: finished')


if __name__ == "__main__":
    run()
