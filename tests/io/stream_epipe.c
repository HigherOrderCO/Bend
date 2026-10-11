#include <sys/wait.h>

// A forked child moves fd onto a pipe whose read end is closed and
// writes a line there with io_out and io_sync, as IO.print does. The
// answer is the child's status as a shell shows it: 128 plus the signal
// that ended it, else its exit code.
Term epipe_write_run(Env e, Term* f, IoWork* w) {
  int fd = (int)(u32)f[0];
  int p[2];
  io_sync();
  pipe(p);
  close(p[0]);
  pid_t pid = fork();
  if (pid == 0) {
    dup2(p[1], fd);
    io_out(fd == 1 ? stdout : stderr, "x\n", 2);
    io_sync();
    _exit(0);
  }
  close(p[1]);
  int s = 0;
  waitpid(pid, &s, 0);
  return (u32)(WIFSIGNALED(s) ? 128 + WTERMSIG(s) : WEXITSTATUS(s));
}

static void __attribute__((constructor)) epipe_write_use(void) {
  io_eff(CID(Epipe.write), epipe_write_run);
}
