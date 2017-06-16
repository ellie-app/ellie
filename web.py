from server.server import app
from server.clock import start_clock
import os

if os.environ['ENV'] == 'production':
    start_clock()
