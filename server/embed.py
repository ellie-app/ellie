import json
from flask import Flask, jsonify, request, render_template, url_for, redirect
from werkzeug.routing import BaseConverter, ValidationError
from datetime import datetime, timedelta
from classes import Version, Constraint, PackageInfo, PackageName, ProjectId, ApiError, Package
import boto3
import botocore
from itertools import *
from operator import *
import storage

app = Flask(__name__)

class ProjectIdConverter(BaseConverter):
    def to_python(self, value):
        return ProjectId.from_string(value)

    def to_url(self, value):
        return str(value)

app.url_map.converters['project_id'] = ProjectIdConverter

@app.errorhandler(ApiError)
def handle_error(error):
    response = jsonify(None)
    response.status_code = error.status_code
    return response

@app.route('/api/upload')
def get_upload_urls():
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
def get_default_revision():
    default_html = packages_cache['data'][PackageName('elm-lang', 'html')][Version(0, 18, 0)]
    default_core = packages_cache['data'][PackageName('elm-lang', 'core')][Version(0, 18, 0)]

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

@app.route('/api/owned')
def get_owned():
    data = storage.get_owned_project_ids()
    print data
    out = jsonify([])
    return out

@app.route('/api/revisions/<project_id:project_id>/<int(min=0):revision_number>')
def get_revision(project_id, revision_number):
    revision = storage.get_revision(project_id, revision_number)
    if revision is None:
        raise ApiError(404)
    return jsonify(revision)

constants = {
    'ENV': 'development',
    'APP_JS': 'http://localhost:8000/app.js',
    'APP_CSS': 'http://localhost:8000/main.css',
    'GTM_ID': '',
    'PROFILE_PIC': 'idk.jpg',
    'CDN_BASE': 'http://localhost:8000',
    'EDITOR_BASE': 'http://localhost:5000',
    'EMBED_BASE': 'http://localhost:5000/embed'
}

@app.route('/')
@app.route('/new')
def new():
    return render_template('new.html', constants=constants)

@app.route('/<project_id:project_id>/<int(min=0):revision_number>')
def existing(project_id, revision_number):
    if project_id.is_old:
        url = url_for('existing', project_id, revision_number)
        return redirect(url, code = 301)

    revision = storage.get_revision(project_id, revision_number)
    if revision is None:
        return redirect('new', code = 303)

    data = {
        'title': revision['title'],
        'description': revision['description'],
        'url': constants['EMBED_BASE'] + '/' + str(project_id) + '/' + str(revision_number)
    }

    return render_template('existing.html', constants=constants, data=data)

# @app.route('/<path:path>')
# def redirect_other_stuff(path):
#     return redirect('new', code = 303)
