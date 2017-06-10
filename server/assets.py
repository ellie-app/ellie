import json
import boto3
import os

_CDN_BASE = os.environ['CDN_BASE']
_PRODUCTION = os.environ['ENV'] == 'production'

with open('./build/editor/manifest.json') as editor_data:
    _editor_manifest = json.load(editor_data)

with open('./build/embed/manifest.json') as embed_data:
    _embed_manifest = json.load(embed_data)


def _prod_asset_path(relative):
    if relative.startswith('embed/'):
        key = relative.replace('embed/', '')
        if key in _embed_manifest:
            return _CDN_BASE + '/assets/embed/' + _embed_manifest[key]
        else:
            return ''
    if relative.startswith('editor/'):
        key = relative.replace('editor/', '')
        if key in _editor_manifest:
            return _CDN_BASE + '/assets/editor/' + _editor_manifest[key]
        else:
            return ''

def _dev_asset_path(relative):
    if relative.startswith('embed/'):
        key = relative.replace('embed/', '')
        return 'http://localhost:8001/' + key
    if relative.startswith('editor/'):
        key = relative.replace('editor/', '')
        return 'http://localhost:8000/' + key

def asset_path(relative):
    if _PRODUCTION:
        return _prod_asset_path(relative)
    else:
        return _dev_asset_path(relative)
