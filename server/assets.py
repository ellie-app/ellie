import json
import os

import boto3

_CDN_BASE = os.environ['CDN_BASE']
_PRODUCTION = os.environ['ENV'] != 'development'

with open('./build/manifest.json') as file_data:
    _manifest = json.load(file_data)


def _prod_asset_path(relative: str) -> str:
    if relative in _manifest:
        return _CDN_BASE + '/assets/' + _manifest[relative]
    else:
        return ''


def _dev_asset_path(relative: str) -> str:
    return 'http://localhost:8000/' + relative


def asset_path(relative: str) -> str:
    if _PRODUCTION:
        return _prod_asset_path(relative)
    else:
        return _dev_asset_path(relative)
