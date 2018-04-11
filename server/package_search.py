import json
import os
import os.path
import re
from datetime import datetime, timedelta
from typing import (Any, Dict, Iterator, List, NamedTuple, Optional, Pattern,
                    Set, TypeVar)

import boto3
from botocore.client import Config
import whoosh.analysis as analysis
import whoosh.fields as fields
import whoosh.index as index
import whoosh.qparser as qparser

from .classes import (Package, PackageInfo, PackageName, ProjectId, Revision,
                      Version)

T = TypeVar('T')

BUCKET_NAME = os.environ['AWS_S3_BUCKET']
INDEX_DIR = ".packages_index"
s3 = boto3.resource('s3', config=Config(signature_version='s3v4'))

_all_compiler_versions = [
    Version(0, 18, 0)
]

_analyzer = analysis.NgramWordAnalyzer(
    2,
    maxsize=None,
    tokenizer=analysis.RegexTokenizer(
        expression=re.compile("[/-]"), gaps=True)
)

_schema = fields.Schema(
    username=fields.TEXT(analyzer=_analyzer, phrase=False, field_boost=1.5),
    package=fields.TEXT(analyzer=_analyzer, phrase=False),
    full_name=fields.TEXT(analyzer=_analyzer, phrase=False),
    full_package=fields.STORED
)


def cat_optionals(data: Iterator[Optional[T]]) -> Iterator[T]:
    for x in data:
        if x is not None:
            yield x


class SearchablePackages(NamedTuple):
    latest_by_elm_version: Dict[Version, Package]
    versions: List[Version]


class PackagesIndex(NamedTuple):
    last_updated: datetime
    indices: Dict[Version, Any]


def build_indices(packages: List[PackageInfo]) -> Dict[Version, Any]:
    indices: Dict[Version, Any] = {}
    for elm_version in _all_compiler_versions:
        idx_path = INDEX_DIR + "/" + str(elm_version)
        if not os.path.exists(idx_path):
            os.makedirs(idx_path)
        idx = index.create_in(idx_path, _schema)
        writer = idx.writer()

        latest_packages: Dict[PackageName, PackageInfo] = {}
        for package_info in packages:
            constraint = package_info.elm_constraint
            if constraint is not None and constraint.is_satisfied(elm_version):
                name = PackageName(package_info.username, package_info.package)
                current = latest_packages.get(name)
                if current is None or package_info.version > current.version:
                    latest_packages[name] = package_info

        for name, info in latest_packages.items():
            writer.add_document(
                username=info.username,
                package=info.package,
                full_name=str(name),
                full_package=Package(name, info.version)
            )

        writer.commit()
        indices[elm_version] = idx
    return indices


def download_searchable_packages() -> List[PackageInfo]:
    body = s3.Object(BUCKET_NAME,
                     'package-artifacts/searchable.json').get()['Body']
    data = body.read()
    body.close()

    return list(
        cat_optionals(
            PackageInfo.from_json(x) for x in json.loads(data)))


def _parse_int(string: str) -> Optional[int]:
    try:
        return int(string)
    except:
        return None


_cache_diff: timedelta = timedelta(minutes=15)

_packages_index: PackagesIndex = PackagesIndex(
    datetime.utcnow(),
    build_indices(download_searchable_packages())
)


def _refresh_packages_cache() -> None:
    global _packages_index
    now = datetime.utcnow()
    if now - _packages_index.last_updated > _cache_diff:
        indices = build_indices(download_searchable_packages())
        _packages_index = PackagesIndex(now, indices)


_parser = qparser.MultifieldParser(["username", "package"], _schema)


def _parse_query(query_string: str) -> Any:
    if "/" in query_string:
        split = query_string.split("/")
        username = split[0]
        package = split[1]
        print(username)
        print(package)
        if username == "" and package != "":
            return _parser.parse("package:" + package)
        elif username != "" and package == "":
            return _parser.parse("username:" + username)
        return _parser.parse("package:" + package + " username:" + username)
    else:
        return _parser.parse(query_string)


def search(elm_version: Version, query_string: str) -> List[Package]:
    _refresh_packages_cache()
    idx = _packages_index.indices.get(elm_version)
    if idx is None:
        return []

    with idx.searcher() as searcher:
        results = searcher.search(_parse_query(query_string), limit=5)
        return [r.fields()['full_package'] for r in results]
