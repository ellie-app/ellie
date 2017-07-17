import json
import multiprocessing
import sys
from typing import Any, NamedTuple

import boto3
import postgresql
from joblib import Parallel, delayed

from .classes import ProjectId

old_client = boto3.client(
    's3',
    aws_access_key_id='AKIAJEOIYZIHVAFXMEMA',
    aws_secret_access_key='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'
)

new_client = boto3.client(
    's3',
    aws_access_key_id='AKIAIIGSNBHHDACF4NMA',
    aws_secret_access_key='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'
)

new_bucket_name = 'production-cdn.ellie-app.com'

db = postgresql.open(
    user='ellie',
    password='humblespark',
    host='ellie.canjzecf31gx.us-east-2.rds.amazonaws.com',
    database='ellie',
    port=5432
)


def transform_package(package: Any) -> Any:
    return (
        package['username'] + '/' + package['name'],
        str(package['version']['major']) + '.' +
        str(package['version']['minor']) + '.' +
        str(package['version']['patch'])
    )


def transform_row(data: Any) -> Any:
    (id, project_id, revision_number, html_code, elm_code, title,
     description, _, snapshot_json, created_at, _, packages_json) = data
    return {
        'id': {
            'projectId': ProjectId(project_id, 1).to_json(),
            'revisionNumber': revision_number
        },
        'htmlCode': html_code,
        'elmCode': elm_code,
        'title': title,
        'description': description,
        'snapshot': json.loads(snapshot_json),
        'created_at': created_at.isoformat(),
        'elmVersion': '0.18.0',
        'packages': [transform_package(p) for p in json.loads(packages_json)]
    }


def transfer(data: Any) -> None:
    if 'path' in data['snapshot']:
        source_key_one = 'compiler-output/' + \
            data['snapshot']['path'] + '.html'
        source_key_two = 'compiler-output/' + data['snapshot']['path']
        source_bucket = 'cdn.ellie-app.com'

        dest_key = 'revisions/' + \
            data['id']['projectId'] + \
            '/' + \
            str(data['id']['revisionNumber']) + '.html'

        try:
            new_client.copy(
                CopySource={'Bucket': source_bucket, 'Key': source_key_one},
                Key=dest_key,
                Bucket=new_bucket_name,
                ExtraArgs={
                    'ContentType': 'text/html',
                    'ACL': 'public-read',
                    'ContentEncoding': 'utf-8'
                },
                SourceClient=old_client
            )
        except:
            new_client.copy(
                CopySource={'Bucket': source_bucket, 'Key': source_key_two},
                Key=dest_key,
                Bucket=new_bucket_name,
                ExtraArgs={
                    'ContentType': 'text/html',
                    'ACL': 'public-read',
                    'ContentEncoding': 'utf-8'
                },
                SourceClient=old_client
            )

    if 'path' in data['snapshot']:
        del data['snapshot']['path']

    new_client.put_object(
        Bucket=new_bucket_name,
        Key='revisions/' + data['id']['projectId'] +
            '/' + str(data['id']['revisionNumber']) + '.json',
        ContentType='application/json',
        ContentEncoding='utf-8',
        ACL='public-read',
        Body=json.dumps(data)
    )


def process_row(row: Any) -> None:
    transfer(transform_row(row))


def run() -> None:
    print('transfer_revisions: starting')

    ps = db.prepare('SELECT * FROM revisions')
    data = ps()
    ps.close()
    db.close()
    total = len(data)
    counter = 0
    for row in data:
        process_row(row)
        counter += 1
        print('transfer_revisions: ' + str(counter) + '/' +
              str(total) + '  ' + str((counter * 100) // total) + '%')

    print('transfer_revisions: finished')
