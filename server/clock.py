from apscheduler.schedulers.blocking import BlockingScheduler
import sync_packages
import os

sched = BlockingScheduler()
SYNC_INTERVAL = int(os.environ['PACKAGE_SYNC_INTERVAL_MINUTES'])

@sched.scheduled_job('interval', minutes=SYNC_INTERVAL)
def run_sync_packages():
    sync_packages.run()

sched.start()
