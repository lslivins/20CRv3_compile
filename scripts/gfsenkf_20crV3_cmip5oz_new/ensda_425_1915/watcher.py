"""
script to babysit reanalysis jobs on hopper.

last modified 20111005
"""
import os, sys, time, smtplib
import email.Message
from subprocess import Popen, PIPE

class Qstat(object):
    def __init__(self, jobid):
        self.jobid = jobid
        p = Popen('qstat %s' % jobid,shell=True,stdin=PIPE,stdout=PIPE,stderr=PIPE)
        qstatlines = p.stdout.readlines()
        if not qstatlines:
            self.exists = False
        else:
            self.exists = True
            qstatout = qstatlines[-1].split()
            self.name = qstatout[1]
            self.user = qstatout[2]
            self.timeused = qstatout[3]
            self.status = qstatout[4]
            self.queue = qstatout[5]

# function to send an email.
def mail(serverURL='localhost', sender='',to='', subject='', text=''):
    message = email.Message.Message()
    message["To"]      = to
    message["From"]    = sender
    message["Subject"] = subject
    message.set_payload(text)
    mailServer = smtplib.SMTP(serverURL)
    mailServer.sendmail(sender, to, message.as_string())
    mailServer.quit()

# user-settable parameters.
# assume expt name is directory name, driver script is ${expt}.csh
expt = os.popen('pwd').read().split('/')[-1][:-1]
scratchdir = os.environ.get('SCRATCH2')
datapath = '%s/%s' % (scratchdir,expt) # master path
timeinterval = 3600 # time interval to check status
emailaddress = 'jeffrey.s.whitaker@noaa.gov'
senderaddress = 'jeffrey.s.whitaker@noaa.gov'
nsubmit_max = 3
driver_script = '%s.csh' % expt

nsubmit = 0
while 1: # go into an infinite loop
    # read job id
    f = open('current_jobid')
    jobid = f.read().split('\n')[0]
    # see if current jobid is running
    f.close()
    is_running = False
    job = Qstat(jobid)
    if job.exists and job.status == 'R':
        is_running = True
    # if job is running, get modification time of analdate.csh file.
    if is_running:
        filename = os.path.join(datapath,'analdate.csh')
        statinfo = os.stat(filename)
        modtime = statinfo[-2]
        # go into a loop, check every 'timeinterval' seconds
        while 1:
            still_running = True
            job = Qstat(jobid)
            if not job.exists or job.status != 'R':
                  still_running = False
            if not still_running: break
            print 'still watching job ...'
            print jobid
            time.sleep(timeinterval)
            statinfo = os.stat(filename)
            modtime_new = statinfo[-2] # modification time is 2nd to last in list
            # if analdate.csh not modified, print a message, email it,
            # delete job and quit.
            # otherwise, keep checking.
            if modtime_new == modtime:
                txt="""
 analdate.csh not modified in last %s seconds
 last modified at %s
 killing job %s and quitting ...""" % (timeinterval,time.ctime(modtime),jobid)
                print txt
                mail(sender=senderaddress, to=emailaddress, subject='job %s on hopper' % jobid, text=txt)
                # kill from shell
                status = os.system('qdel %s' % jobid)
                # now we resubmit
                # nsubmit initialized to zero when watcher.py started.
                if nsubmit < nsubmit_max:
                    f = open('current_jobid')
                    jobidnew = f.read().split('\n')[0]
                    f.close()
                    if jobid == jobidnew:
                        # run qsub, capture stdout.
                        stdin,stdout = os.popen4('qsub %s' % driver_script)
                        # write stdout to current_jobid file.
                        fout = open('current_jobid','w')
                        fout.write(stdout.readline())
                        fout.close()
                    else:
                        print "job id changed"
                        #nsubmit = 0
                        break
                    # increment nsubmit counter.
                    nsubmit = nsubmit + 1
                    print "job resubmitted for %s th time" % nsubmit
                    break
                else:
                    raise SystemExit('max number of resubmissions exceeded')
            modtime = modtime_new
    else:  # if job not running...
        print 'job ',jobid,' not running at ',time.ctime()
        # if job exists but is queued, wait timeinterval seconds and check again.
        if job.exists:
            print 'job state = ',job.status
            if job.status == 'Q':
                print 'still watching job ...'
                print jobid
                time.sleep(timeinterval)
        else: # if job doesn't exist, quit.
            #print 'job does not exist, quitting...'
            #raise SystemExit
            print 'job does not exist, resubmitting...'
            # run qsub, capture stdout.
            if nsubmit < nsubmit_max:
                stdin,stdout = os.popen4('qsub %s' % driver_script)
                # write stdout to current_jobid file.
                fout = open('current_jobid','w')
                fout.write(stdout.readline())
                fout.close()
                # increment nsubmit counter.
                nsubmit = nsubmit + 1
                print "job resubmitted for %s th time" % nsubmit
            else:
                print "max resubmit limit reached, quitting..."
                raise SystemExit
