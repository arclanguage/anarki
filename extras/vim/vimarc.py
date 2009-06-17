# Go-between to allow regular repl access + spit pipe input from vim into the repl.
# scott.vimarc@h4ck3r.net, 2008.

import pexpect, os, sys, tty, select

lispCmd = '../arc.sh' # or, 'sbcl', or whatever should probably work
pipeLoc = os.path.expanduser("~/.vimarc-pipe") # this path has to be the same as in vimarc.vim
if not os.path.exists(pipeLoc):
    os.system("mkfifo -m go-rwx " + pipeLoc)

class Funnel(pexpect.spawn):
    """ hacky monkey patch of pexpect to merge `interact' and input from a pipe. spawn the lisp using
        this command, and then vim connects to the pipe and you can still see/use the lisp repl in your
        shell window."""

    def mergePipeAndInteract(self, pipe):
        self.stdout.write (self.buffer)
        self.stdout.flush()
        self.buffer = ''
        mode = tty.tcgetattr(self.STDIN_FILENO)
        tty.setraw(self.STDIN_FILENO)
        try:
            self.__merge_copy(pipe)
        finally:
            tty.tcsetattr(self.STDIN_FILENO, tty.TCSAFLUSH, mode)

    def __interact_writen(self, fd, data):
        while data != '' and self.isalive():
            n = os.write(fd, data)
            data = data[n:]
    def __interact_read(self, fd):
        return os.read(fd, 1000)

    def __select (self, iwtd, owtd, ewtd, timeout=None):
        # if select() is interrupted by a signal (errno==EINTR) then
        # we loop back and enter the select() again.
        if timeout is not None:
            end_time = time.time() + timeout 
        while True:
            try:
                return select.select (iwtd, owtd, ewtd, timeout)
            except select.error, e:
                if e[0] == errno.EINTR:
                    # if we loop back we have to subtract the amount of time we already waited.
                    if timeout is not None:
                        timeout = end_time - time.time()
                        if timeout < 0:
                            return ([],[],[])
                else: # something else caused the select.error, so this really is an exception
                    raise

    def __merge_copy(self, pipe):
        while self.isalive():
            r,w,e = self.__select([self.child_fd, self.STDIN_FILENO, pipe], [], [])
            if self.child_fd in r:
                data = self.__interact_read(self.child_fd)
                os.write(self.STDOUT_FILENO, data)
            if self.STDIN_FILENO in r:
                data = self.__interact_read(self.STDIN_FILENO)
                self.__interact_writen(self.child_fd, data)
            if pipe in r:
                data = self.__interact_read(pipe)
                self.__interact_writen(self.child_fd, data)

f = Funnel(lispCmd, logfile=sys.stdout)
pipe = open(pipeLoc, "r+")
pipefn = pipe.fileno()
try:
    f.mergePipeAndInteract(pipefn)
except OSError:
    pass
