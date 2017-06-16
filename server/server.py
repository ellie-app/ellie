from typing import Optional, Any, Optional, Iterator, TypeVar, List, NamedTuple, Dict, Tuple
import json
from flask import Flask, jsonify, request, render_template, url_for, redirect
from werkzeug.routing import BaseConverter, ValidationError
from datetime import datetime, timedelta
import boto3
import botocore
from itertools import *
from operator import *
from urllib.parse import urlparse
import os
from . import storage
from . import assets
from .classes import Version, Constraint, PackageInfo, PackageName, ProjectId, ApiError, Package

T = TypeVar('T')

def cat_optionals(data: Iterator[Optional[T]]) -> Iterator[T]:
    for x in data:
        if x is not None:
            yield x

app = Flask(__name__)

class ProjectIdConverter(BaseConverter):
    def to_python(self, value: str) -> Optional[ProjectId]:
        return ProjectId.from_string(value)

    def to_url(self, value: ProjectId) -> str:
        return str(value)

app.url_map.converters['project_id'] = ProjectIdConverter

@app.errorhandler(ApiError)
def handle_error(error: ApiError) -> Any:
    response = jsonify(None)
    response.status_code = error.status_code
    return response

def parse_int(string: str) -> Optional[int]:
    try:
        return int(string)
    except:
        return None

def package_entry_score(key: PackageName, entries: Dict[Version, Package], elm_version: Version, query: str) -> Tuple[Optional[Package], float]:
    if elm_version not in entries:
        return (None, 0)

    score = 0.0
    package = entries[elm_version]

    username_query = query
    project_query = query
    if "/" in query:
        [username_query, project_query] = query.split("/")

    if project_query in key.project:
        score += len(project_query) - (key.project.index(project_query) / 2)
    else:
        return (None, 0)

    if username_query in key.user:
        score += (len(username_query) * 1.5) - (key.user.index(username_query) / 2)

    return (entries[elm_version], score)

def do_search(elm_version: Version, query: str) -> List[Any]:
    cache_data = storage.get_searchable_packages()
    score_gen = (package_entry_score(key, info.latest_by_elm_version, elm_version, query) for key, info in cache_data.items())
    filtered = filter(lambda t: t[1] > 0, score_gen)
    sorted_gen = sorted(list(filtered), key = lambda t: -t[1])
    return [p.to_json() for p, s in sorted_gen if p is not None][0:5]

@app.route('/api/packages/<string:user>/<string:project>/versions')
def tags(user: str, project: str) -> Any:
    cache_data = storage.get_searchable_packages()
    key = PackageName(user, project)

    if key not in cache_data:
        raise ApiError(404)

    return jsonify([v.to_json() for v in cache_data[key].versions])

@app.route('/api/search')
def search() -> Any:
    query = request.args.get('query')
    if not isinstance(query, str):
        raise(ApiError(400))
    if len(query) <= 3:
        return jsonify([])

    elm_version = request.args.get('elmVersion')
    if not isinstance(elm_version, str):
        raise ApiError(400)
    parsed_elm_version = Version.from_string(elm_version)
    if parsed_elm_version is None:
        raise ApiError(400)

    packages = do_search(parsed_elm_version, query)

    return jsonify(packages)


@app.route('/api/upload')
def get_upload_urls() -> Any:
    project_id_string = request.args.get('projectId')

    project_id = ProjectId.from_string(project_id_string) if (project_id_string is not None) else ProjectId.generate()
    if project_id is None:
        raise ApiError(400)

    if project_id_string is not None and not storage.revision_exists(project_id, 0):
        raise ApiError(404)

    if project_id_string is not None and not storage.project_id_is_owned(project_id):
        raise ApiError(403)

    revision_string = request.args.get('revisionNumber')
    if project_id_string is not None and revision_string is None:
        raise ApiError(400)

    revision_number = parse_int(revision_string) if (revision_string is not None) else 0
    if revision_number is None:
        raise ApiError(400)
    if revision_number < 0:
        raise ApiError(400)

    if storage.revision_exists(project_id, revision_number):
        raise ApiError(400)

    response = jsonify({
        'revision': storage.get_revision_upload_signature(project_id, revision_number),
        'result': storage.get_result_upload_signature(project_id, revision_number)
    })

    storage.add_project_id_ownership(project_id, response)

    return response

@app.route('/api/revisions/default')
def get_default_revision() -> Any:
    cache_data = storage.get_searchable_packages()
    default_html = cache_data[PackageName('elm-lang', 'html')].latest_by_elm_version[Version(0, 18, 0)]
    default_core = cache_data[PackageName('elm-lang', 'core')].latest_by_elm_version[Version(0, 18, 0)]

    return jsonify({
        'packages': [default_core.to_json(), default_html.to_json()],
        'projectId': None,
        'revisionNumber': None,
        'title': 'Untitled',
        'id': None,
        'description': 'Tell the world about your project!',
        'elmCode': '''module Main exposing (main)

import Html exposing (Html, text)


main : Html msg
main =
    text "Hello, World!"
''',
        'htmlCode': '''<html>
<head>
  <style>
    html {
      background: #F7F7F7;
      color: red;
    }
  </style>
</head>
<body>
  <script>
    var app = Elm.Main.fullscreen()
  </script>
</body>
</html>
'''
    })

@app.route('/api/revisions/<project_id:project_id>/<int(min=0):revision_number>')
def get_revision(project_id: ProjectId, revision_number: int) -> Any:
    revision = storage.get_revision(project_id, revision_number)
    if revision is None:
        raise ApiError(404)
    return jsonify(revision)

EDITOR_CONSTANTS = {
    'ENV': os.environ['ENV'],
    'APP_JS': assets.asset_path('editor/app.js'),
    'APP_CSS': assets.asset_path('editor/app.css'),
    'GTM_ID': os.environ['GTM_ID'],
    'PROFILE_PIC': 'idk.jpg',
    'CDN_BASE': os.environ['CDN_BASE'],
    'SERVER_HOSTNAME': os.environ['SERVER_HOSTNAME']
}

@app.route('/')
@app.route('/new')
def new() -> Any:
    return render_template('new.html', constants=EDITOR_CONSTANTS)

@app.route('/<project_id:project_id>/<int(min=0):revision_number>')
def existing(project_id: ProjectId, revision_number: int) -> Any:
    if project_id.is_old:
        url = url_for('existing', project_id, revision_number)
        return redirect(url, code = 301)

    revision = storage.get_revision(project_id, revision_number)
    if revision is None:
        return redirect('new', code = 303)

    data = {
        'title': revision.title,
        'description': revision.description,
        'url': EDITOR_CONSTANTS['SERVER_HOSTNAME'] + '/' + str(project_id) + '/' + str(revision_number)
    }

    return render_template('existing.html', constants=EDITOR_CONSTANTS, data=data)


EMBED_CONSTANTS = {
    'ENV': os.environ['ENV'],
    'APP_JS': assets.asset_path('embed/app.js'),
    'APP_CSS': assets.asset_path('embed/app.css'),
    'GTM_ID': os.environ['GTM_ID'],
    'PROFILE_PIC': 'idk.jpg',
    'CDN_BASE': os.environ['CDN_BASE'],
    'SERVER_HOSTNAME': os.environ['SERVER_HOSTNAME']
}

@app.route('/embed/<project_id:project_id>/<int(min=0):revision_number>')
def embed(project_id: ProjectId, revision_number: int) -> Any:
    if project_id.is_old:
        url = url_for('embed', project_id, revision_number)
        return redirect(url, code = 301)

    data = {}
    revision = storage.get_revision(project_id, revision_number)
    if revision is not None:
        data['title'] = revision.title
        data['description'] = revision.description
        data['url'] = EMBED_CONSTANTS['SERVER_HOSTNAME'] + '/embed/' + str(project_id) + '/' + str(revision_number)

    return render_template('embed.html', constants=EMBED_CONSTANTS, data=data)

@app.route('/oembed')
def oembed() -> Any:
    url = request.args.get('url')
    width_param = parse_int(request.args.get('width'))
    height_param = parse_int(request.args.get('height'))

    width = width_param if width_param is not None else 800
    height = height_param if height_param is not None else 400

    parsed_url = urlparse(url)
    if not parsed_url.hostname or 'ellie-app.com' not in parsed_url.hostname or not parsed_url.path:
        raise ApiError(404)

    split = parsed_url.path.split('/')
    if len(split) != 3:
        raise ApiError(404)

    [_, project_id_str, revision_number_str] = split

    project_id = ProjectId.from_string(project_id_str)
    if project_id is None:
        raise ApiError(404)

    revision_number = parse_int(revision_number_str)
    if revision_number is None:
        raise ApiError(404)

    revision = storage.get_revision(project_id, revision_number)
    if revision is None:
        raise ApiError(404)

    return jsonify({
        'width': 'width',
        'height': 'height',
        'type': 'rich',
        'version': '1.0',
        'title': revision.title,
        'provider_name': 'ellie-app.com',
        'provider_url': 'https://ellie-app.com',
        'html': '<iframe src="' + EDITOR_CONSTANTS['SERVER_HOSTNAME'] + '/embed/' + str(project_id) + '/' + str(revision_number) + '" width=' + str(width) + ' height=' + str(height) + ' frameBorder="0" allowtransparency="true"></iframe>'
    })
