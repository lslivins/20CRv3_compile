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

if __name__ == "__main__":
    import sys
    job = Qstat(sys.argv[1])
    for k,v in job.__dict__.iteritems():
        print '%s: %s' % (k,v)
