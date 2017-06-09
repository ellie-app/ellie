import boto3
import botocore
import json
from flask import request
from hmac import new as hmac
from hashlib import sha256
import base64
import re
import urllib
from classes import ProjectId
import os

BUCKET_NAME = os.environ['AWS_S3_BUCKET']

cookie_key = "asf00982h3)(FEAH90)(qy39rALHFA*YTP(WQ#T*(Tyqgifubfa)(Y#9  12-)))"
def _sign_cookie(value):
    mac = hmac(cookie_key, None, sha256)
    mac.update(value)
    b64 = base64.b64encode(mac.digest())
    replaced = re.sub(r'\=+$', '', b64)
    return urllib.quote('s:' + value + '.' + replaced, safe='')

def _unsign_cookie(value):
    unsigned_value = urllib.unquote(value[0 : value.rfind('.')]).replace('s:', '')
    to_match = _sign_cookie(unsigned_value)
    return unsigned_value if to_match == value else None

s3 = boto3.resource('s3')
client = boto3.client('s3')
bucket = s3.Bucket(BUCKET_NAME)

def _get_owned_project_ids():
    raw = request.cookies.get('ownedProjects')
    if raw is None:
        return set()

    unsigned = _unsign_cookie(raw)
    if unsigned is None:
        return set()

    try:
        return set(ProjectId.from_string(x) for x in json.loads(unsigned))
    except:
        return set()


def project_id_is_owned(project_id):
    return project_id in _get_owned_project_ids()

def add_project_id_ownership(project_id, response):
    project_ids = _get_owned_project_ids()
    project_ids.add(project_id)
    cookie_string = json.dumps([str(x) for x in project_ids])
    response.set_cookie('ownedProjects', _sign_cookie(cookie_string))

def get_revision(project_id, revision_number):
    try:
        key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.json'
        data = client.get_object(Bucket = BUCKET_NAME, Key = key)
        body = data['Body']
        output = json.loads(body.read())
        output['owned'] = project_id_is_owned(project_id)

        body.close()
        return output
    except botocore.exceptions.ClientError as e:
        if e.response['Error']['Code'] == "404":
            return None
        else:
            raise

def revision_exists(project_id, revision_number):
    try:
        key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.json'
        client.head_object(Bucket = BUCKET_NAME, Key = key)
        return True
    except botocore.exceptions.ClientError as e:
        if e.response['Error']['Code'] == "404":
            return False
        else:
            raise


def get_revision_upload_signature(project_id, revision_number):
    data = client.generate_presigned_post(
        Bucket = BUCKET_NAME,
        Key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.json',
        Fields = {
            'acl': 'public-read',
            'Content-Type': 'application/json'
        },
        Conditions = [
            { 'acl': 'public-read' },
            { 'Content-Type': 'application/json' }
        ]
    )

    data['projectId'] = str(project_id)
    data['revisionNumber'] = revision_number
    return data

def get_result_upload_signature(project_id, revision_number):
    data = client.generate_presigned_post(
        Bucket = BUCKET_NAME,
        Key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.html',
        Fields = {
            'acl': 'public-read',
            'Content-Type': 'text/html'
        },
        Conditions = [
            { 'acl': 'public-read' },
            { 'Content-Type': 'text/html' }
        ]
    )

    data['projectId'] = str(project_id)
    data['revisionNumber'] = revision_number
    return data
