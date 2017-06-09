import json
import boto3
import os

with open('./build/editor/manifest.json') as editor_data:
    editor_manifest = json.load(editor_data)

with open('./build/embed/manifest.json') as embed_data:
    embed_manifest = json.load(embed_data)

client = boto3.client('s3')

for key, value in editor_manifest.iteritems():
    print ('deploying editor/' + key)
    client.upload_file(
        './build/editor/' + value,
        os.environ['AWS_S3_BUCKET'],
        'assets/editor/' + value,
        ExtraArgs = {
            'ACL': 'public-read',
            'ContentType': 'text/css' if key.endswith('.css') else 'application/javascript'
        }
    )

for key, value in embed_manifest.iteritems():
    print ('deploying embed/' + key)
    client.upload_file(
        './build/embed/' + value,
        os.environ['AWS_S3_BUCKET'],
        'assets/embed/' + value,
        ExtraArgs = {
            'ACL': 'public-read',
            'ContentType': 'text/css' if key.endswith('.css') else 'application/javascript'
        }
    )

print 'asset deploy complete'
