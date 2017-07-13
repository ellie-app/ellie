import logging
import os
import threading

from apscheduler.schedulers.blocking import BlockingScheduler

from . import sync_packages

logging.basicConfig()

SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])
sched = BlockingScheduler()


def run_in_thread() -> None:
    sync_packages.run()


@sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
def run_sync_packages() -> None:
    thread = threading.Thread(target=run_in_thread)
    thread.start()


def start() -> None:
    sched.start()
