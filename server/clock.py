from apscheduler.schedulers.blocking import BlockingScheduler
import sync_packages

sched = BlockingScheduler()

@sched.scheduled_job('interval', minutes=15)
def run_sync_packages():
    sync_packages.run()

sched.start()
