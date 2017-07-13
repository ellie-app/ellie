import logging
import os
import threading

from apscheduler.schedulers.background import BackgroundScheduler

from . import sync_packages

logging.basicConfig()

SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])
sched = BackgroundScheduler()


@sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
def run_sync_packages() -> None:
    sync_packages.run()


def start() -> None:
    print('starting')
    sched.start()
