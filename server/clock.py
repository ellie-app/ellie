import os

from apscheduler.schedulers.background import BackgroundScheduler

from . import sync_packages

SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])
sched = BackgroundScheduler(daemon=False)


@sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
def run_sync_packages() -> None:
    sync_packages.run()


def start() -> None:
    print('starting')
    sched.start()
