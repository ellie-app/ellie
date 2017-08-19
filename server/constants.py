import os

LATEST_TERMS_VERSION = 1
PRODUCTION = os.environ.get('ENV') == 'production'
