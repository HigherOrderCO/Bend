#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

// A File on the read end of a FIFO whose writer sends n bytes one at a
// time. A single read(2) answers the first byte; a read that fills its
// buffer answers all n.
static void short_file_call(IoWork* w) {
  char path[64];
  snprintf(path, sizeof path, "/tmp/bend_read_short_%d", (int)getpid());
  unlink(path);
  mkfifo(path, 0600);
  int rd = open(path, O_RDONLY | O_NONBLOCK);
  int wr = open(path, O_WRONLY | O_NONBLOCK);
  unlink(path);
  pid_t pid = fork();
  if (pid == 0) {
    close(rd);
    for (u32 i = 0; i < w->word; i += 1) {
      write(wr, "x", 1);
      usleep(1000);
    }
    close(wr);
    _exit(0);
  }
  close(wr);
  int flags = fcntl(rd, F_GETFL);
  fcntl(rd, F_SETFL, flags & ~O_NONBLOCK);
  w->hand = rd;
}

static Term short_file_pack(Env e, IoWork* w) {
  return io_hand(w->hand);
}

Term short_file_run(Env e, Term* f, IoWork* w) {
  w->word = (u32)f[0];
  return io_work(w, short_file_call, short_file_pack);
}

static void __attribute__((constructor)) short_file_use(void) {
  io_eff(CID(short.file), short_file_run);
}
