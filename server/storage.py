import base64
import json
import os
import re
from datetime import datetime, timedelta
from hashlib import sha256
from hmac import new as hmac
from typing import (Any, Dict, Iterator, List, NamedTuple, Optional, Pattern,
                    Set, TypeVar)
from urllib.parse import quote, unquote

import boto3
import botocore
from botocore.client import Config
from flask import request

from .classes import (Package, PackageInfo, PackageName, ProjectId, Revision,
                      Version)

BUCKET_NAME = os.environ['AWS_S3_BUCKET']

T = TypeVar('T')


def cat_optionals(data: Iterator[Optional[T]]) -> Iterator[T]:
    for x in data:
        if x is not None:
            yield x


cookie_key = os.environ['COOKIE_SECRET'].encode('utf-8')
replace_re: Pattern = re.compile('\=+$')


def _sign_cookie(value: str) -> str:
    mac = hmac(cookie_key, msg=None, digestmod=sha256)
    mac.update(value.encode('utf-8'))
    b64 = base64.b64encode(mac.digest())
    replaced = b64.decode('utf-8').rstrip('=')
    return quote('s:' + value + '.' + replaced, safe='')


def _unsign_cookie(value: str) -> Optional[str]:
    unsigned_value = unquote(value[0:value.rfind('.')]).replace('s:', '')
    to_match = _sign_cookie(unsigned_value)
    return unsigned_value if to_match == value else None


s3 = boto3.resource('s3', config=Config(signature_version='s3v4'))
client = boto3.client('s3', config=Config(signature_version='s3v4'))
bucket = s3.Bucket(BUCKET_NAME)


def _get_owned_project_ids() -> Set[ProjectId]:
    raw = request.cookies.get('ownedProjects')
    if raw is None:
        return set()

    unsigned = _unsign_cookie(raw)
    if unsigned is None:
        return set()

    try:
        return set(
            cat_optionals(
                ProjectId.from_string(x) for x in json.loads(unsigned)))
    except:
        return set()


def project_id_is_owned(project_id: ProjectId) -> bool:
    return project_id in _get_owned_project_ids()


def add_project_id_ownership(project_id: ProjectId, response: Any) -> None:
    project_ids = _get_owned_project_ids()
    project_ids.add(project_id)
    cookie_string = json.dumps([str(x) for x in project_ids])
    response.set_cookie('ownedProjects', _sign_cookie(cookie_string))


def get_revision(project_id: ProjectId,
                 revision_number: int) -> Optional[Revision]:
    try:
        key = 'revisions/' + str(project_id) + '/' + str(
            revision_number) + '.json'
        data = client.get_object(Bucket=BUCKET_NAME, Key=key)
        body = data['Body']
        json_data = json.loads(body.read())
        json_data['owned'] = project_id_is_owned(project_id)
        revision = Revision.from_json(json_data)
        body.close()
        return revision
    except Exception as e:
        return None


def revision_exists(project_id: ProjectId, revision_number: int) -> bool:
    try:
        key = 'revisions/' + str(project_id) + '/' + str(
            revision_number) + '.json'
        client.head_object(Bucket=BUCKET_NAME, Key=key)
        return True
    except:
        return False


def get_revision_upload_signature(project_id: ProjectId,
                                  revision_number: int) -> Any:
    data = client.generate_presigned_post(
        Bucket=BUCKET_NAME,
        Key='revisions/' + str(project_id) + '/' + str(revision_number) +
        '.json',
        Fields={'acl': 'public-read',
                'Content-Type': 'application/json'},
        Conditions=[{
            'acl': 'public-read'
        }, {
            'Content-Type': 'application/json'
        }])

    data['projectId'] = str(project_id)
    data['revisionNumber'] = revision_number
    return data


def get_result_upload_signature(project_id: ProjectId,
                                revision_number: int) -> Any:
    data = client.generate_presigned_post(
        Bucket=BUCKET_NAME,
        Key='revisions/' + str(project_id) + '/' + str(revision_number) +
        '.html',
        Fields={'acl': 'public-read',
                'Content-Type': 'text/html'},
        Conditions=[{
            'acl': 'public-read'
        }, {
            'Content-Type': 'text/html'
        }])

    data['projectId'] = str(project_id)
    data['revisionNumber'] = revision_number
    return data


all_versions = [
    Version(0, 18, 0),
    Version(0, 17, 1),
    Version(0, 17, 0),
    Version(0, 16, 0),
    Version(0, 15, 0)
]


class SearchablePackages(NamedTuple):
    latest_by_elm_version: Dict[Version, Package]
    versions: List[Version]


class PackagesCache(NamedTuple):
    last_updated: datetime
    data: Dict[PackageName, SearchablePackages]


def organize_packages(
        packages: List[PackageInfo]) -> Dict[PackageName, SearchablePackages]:
    data: Dict[PackageName, SearchablePackages] = {}
    for package in packages:
        key = PackageName(package.username, package.package)
        if key not in data:
            data[key] = SearchablePackages({}, [])

        data[key].versions.append(package.version)

        for version in all_versions:
            if package.elm_constraint is not None and package.elm_constraint.is_satisfied(
                    version):
                if version not in data[key].latest_by_elm_version:
                    data[key].latest_by_elm_version[
                        version] = package.to_package()
                if data[key].latest_by_elm_version[version].version < package.version:
                    data[key].latest_by_elm_version[
                        version] = package.to_package()
    return data


def download_searchable_packages() -> Dict[PackageName, SearchablePackages]:
    body = s3.Object(BUCKET_NAME,
                     'package-artifacts/searchable.json').get()['Body']
    data = body.read()
    packages = list(
        cat_optionals(PackageInfo.from_json(x) for x in json.loads(data)))
    body.close()
    return organize_packages(packages)


def parse_int(string: str) -> Optional[int]:
    try:
        return int(string)
    except:
        return None


cache_diff: timedelta = timedelta(minutes=15)

packages_cache: PackagesCache = PackagesCache(datetime.utcnow(),
                                              download_searchable_packages())


def refresh_packages_cache() -> None:
    global packages_cache
    now = datetime.utcnow()
    if now - packages_cache.last_updated > cache_diff:
        packages_cache = PackagesCache(now, download_searchable_packages())


def get_searchable_packages() -> Dict[PackageName, SearchablePackages]:
    refresh_packages_cache()
    return packages_cache.data
