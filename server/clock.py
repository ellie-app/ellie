import logging
from apscheduler.schedulers.blocking import BlockingScheduler
import sync_packages
import os
import subprocess

logging.basicConfig()

sched = BlockingScheduler()
SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])

@sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
def run_sync_packages() -> None:
    subprocess.check_call(["python", "./server/sync_packages.py"])

def start_clock() -> None:
    sched.start()
