import logging
from apscheduler.schedulers.blocking import BlockingScheduler
from . import sync_packages
import threading
import os

logging.basicConfig()

def my_threaded_func() -> None:
    SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])
    sched = BlockingScheduler()

    @sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
    def run_sync_packages() -> None:
        sync_packages.run()

    sched.start()

def start() -> None:
    thread = threading.Thread(target=my_threaded_func)
    thread.start()
