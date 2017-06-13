from typing import Pattern, Optional, Set, Any, TypeVar, Iterator, List
import boto3
import botocore
import json
from flask import request
from hmac import new as hmac
from hashlib import sha256
import base64
import re
from urllib.parse import quote, unquote
import os
from .classes import ProjectId, Revision

BUCKET_NAME = os.environ['AWS_S3_BUCKET']

T = TypeVar('T')

def cat_optionals(data: Iterator[Optional[T]]) -> Iterator[T]:
    for x in data:
        if x is not None:
            yield x

cookie_key = "asf00982h3)(FEAH90)(qy39rALHFA*YTP(WQ#T*(Tyqgifubfa)(Y#9  12-)))".encode('utf-8')
replace_re : Pattern = re.compile('\=+$')
def _sign_cookie(value: str) -> str:
    mac = hmac(cookie_key, msg = None, digestmod = sha256)
    mac.update(value.encode('utf-8'))
    b64 = base64.b64encode(mac.digest())
    replaced = re.sub(replace_re, '', b64)
    return quote('s:' + value + '.' + replaced, safe='')

def _unsign_cookie(value: str) -> Optional[str]:
    unsigned_value = unquote(value[0 : value.rfind('.')]).replace('s:', '')
    to_match = _sign_cookie(unsigned_value)
    return unsigned_value if to_match == value else None

s3 = boto3.resource('s3')
client = boto3.client('s3')
bucket = s3.Bucket(BUCKET_NAME)

def _get_owned_project_ids() -> Set[ProjectId]:
    raw = request.cookies.get('ownedProjects')
    if raw is None:
        return set()

    unsigned = _unsign_cookie(raw)
    if unsigned is None:
        return set()

    try:
        return set(cat_optionals(ProjectId.from_string(x) for x in json.loads(unsigned)))
    except:
        return set()


def project_id_is_owned(project_id: ProjectId) -> bool:
    return project_id in _get_owned_project_ids()

def add_project_id_ownership(project_id: ProjectId, response: Any) -> None:
    project_ids = _get_owned_project_ids()
    project_ids.add(project_id)
    cookie_string = json.dumps([str(x) for x in project_ids])
    response.set_cookie('ownedProjects', _sign_cookie(cookie_string))

def get_revision(project_id: ProjectId, revision_number: int) -> Optional[Revision]:
    try:
        key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.json'
        data = client.get_object(Bucket = BUCKET_NAME, Key = key)
        body = data['Body']
        json_data = json.loads(body.read())
        json_data['owned'] = project_id_is_owned(project_id)
        revision = Revision.from_json(json_data)
        body.close()
        return revision
    except:
        return None

def revision_exists(project_id: ProjectId, revision_number: int) -> bool:
    try:
        key = 'revisions/' + str(project_id) + '/' + str(revision_number) + '.json'
        client.head_object(Bucket = BUCKET_NAME, Key = key)
        return True
    except:
        return False


def get_revision_upload_signature(project_id: ProjectId, revision_number: int) -> Any:
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

def get_result_upload_signature(project_id: ProjectId, revision_number: int) -> Any:
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
