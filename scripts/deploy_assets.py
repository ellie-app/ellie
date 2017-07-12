import json
import boto3
import os

with open('./build/manifest.json') as file_data:
    manifest = json.load(file_data)

client = boto3.client('s3')

for key, value in manifest.items():
    print('deploying ' + key)
    client.upload_file(
        './build/' + value,
        os.environ['AWS_S3_BUCKET'],
        'assets/' + value,
        ExtraArgs={
            'ACL':
            'public-read',
            'ContentType':
            'text/css' if key.endswith('.css') else 'application/javascript'
        })

print('asset deploy complete')
