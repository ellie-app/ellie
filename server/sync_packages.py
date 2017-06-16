from typing import List, Optional, Any, Tuple, Set
import urllib2
from urllib2 import URLError, HTTPError
import json
import tempfile
import os
import zipfile
import shutil
import glob2
from joblib import Parallel, delayed
import multiprocessing
import boto3
import sys
from datetime import datetime
from classes import Version, Constraint, PackageInfo
import os
from . import storage

BUCKET_NAME = os.environ['AWS_S3_BUCKET']

s3 = boto3.resource('s3')
bucket = s3.Bucket(BUCKET_NAME)

def glob_all(paths: List[str]) -> List[str]:
    output: List[str] = []
    for path in paths:
        output = output + glob2.glob(path)
    return output

def download_packages() -> Any:
    body = urllib2.urlopen("http://package.elm-lang.org/all-packages")
    data = json.loads(body.read())
    body.close()
    return data

def organize_packages(data: Any) -> List[PackageInfo]:
    output = []
    for entry in data:
        username = entry['name'].split('/')[0]
        package = entry['name'].split('/')[1]
        for version in entry['versions']:
            output.append(PackageInfo(username, package, Version.from_string(version)))
    return output

def make_temp_directory() -> str:
    return tempfile.mkdtemp(prefix='ellie-package-temp-')

def download_package_zip(base_dir: str, package: PackageInfo) -> None:
    url = 'https://github.com/' + package.username + '/' + package.package + '/archive/' + str(package.version) + '.zip'
    zip_path = os.path.join(base_dir, 'temp.zip')
    try:
        request = urllib2.urlopen(url)
        with open(zip_path, "w+") as local_file:
            local_file.write(request.read())
    except HTTPError as e:
        print("HTTP Error:", e.code, url)
    except URLError as e:
        print("URL Error:", e.reason, url)

def unzip_and_delete(base_dir: str) -> None:
    zip_path = os.path.join(base_dir, 'temp.zip')
    zip_ref = zipfile.ZipFile(zip_path, 'r')
    zip_ref.extractall(base_dir)
    zip_ref.close()
    os.remove(zip_path)

def read_package_json(base_dir: str, package: PackageInfo) -> Any:
    package_json_path = os.path.join(base_dir, package.package + '-' + str(package.version), 'elm-package.json')
    return json.loads(open(package_json_path, 'r').read())

def read_source_files(base_dir: str, package: PackageInfo, package_json: Any) -> Any:
    package_dir = os.path.join(base_dir, package.package + '-' + str(package.version))
    nested_elm = [os.path.join(package_dir, a, '**/*.elm') for a in package_json['source-directories']]
    nested_js = [os.path.join(package_dir, a, '**/*.js') for a in package_json['source-directories']]
    filenames = glob_all(nested_elm + nested_js)
    output = {}
    for filename in filenames:
        output[filename.replace(package_dir + '/', '')] = open(filename, 'r').read()
    return output

def get_last_updated() -> int:
    body = s3.Object(BUCKET_NAME, 'package-artifacts/last-updated').get()['Body']
    data = body.read()
    time = json.loads(data)
    body.close()
    return time

def get_current_time() -> int:
    epoch = datetime.utcfromtimestamp(0)
    dt = datetime.utcnow()
    return int((dt - epoch).total_seconds() * 1000.0)

def process_package(package: PackageInfo) -> Tuple[bool, PackageInfo]:
    try:
        base_dir = make_temp_directory()
        download_package_zip(base_dir, package)
        unzip_and_delete(base_dir)
        package_json = read_package_json(base_dir, package)
        source_files = read_source_files(base_dir, package, package_json)
        bucket.put_object(
            Key = package.s3_package_key(),
            ACL = 'public-read',
            Body = json.dumps(package_json).encode('utf-8'),
            ContentType = 'application/json'
        )
        bucket.put_object(
            Key = package.s3_source_key(),
            ACL = 'public-read',
            Body = json.dumps(source_files).encode('utf-8'),
            ContentType = 'application/json'
        )
        shutil.rmtree(base_dir)
        package.set_elm_constraint(Constraint.from_string(package_json['elm-version']))
        return (True, package)
    except:
        print(package)
        print(sys.exc_info())
        return (False, package)

def upload_searchable_packages(packages: List[PackageInfo]) -> None:
    bucket.put_object(
        Key = 'package-artifacts/searchable.json',
        ACL = 'public-read',
        Body = json.dumps([x.to_json() for x in packages]).encode('utf-8'),
        ContentType = 'application/json'
    )

def download_searchable_packages() -> Set[PackageInfo]:
    try:
        body = s3.Object(BUCKET_NAME, 'package-artifacts/searchable.json').get()['Body']
        data = body.read()
        packages = set(PackageInfo.from_json(x) for x in json.loads(data))
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
    package_groups = [filtered_packages[x : x + num_cores] for x in range(0, len(filtered_packages), num_cores)]
    counter = 0.0
    total = float(len(filtered_packages))
    failed = []
    for package_group in package_groups:
        results = Parallel(n_jobs = num_cores)(delayed(process_package)(i) for i in package_group)
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
