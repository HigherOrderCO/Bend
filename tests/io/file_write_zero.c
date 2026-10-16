// bend-ids 1
// CID :Stall.open BEND_CID_Stall_open
// bend-ids end
#include <dlfcn.h>

// Every nonempty write to stall_fd makes no progress; the rest go through.
static int stall_fd = -1;

ssize_t write(int fd, const void* data, size_t size) {
  if (fd == stall_fd && size > 0) {
    return 0;
  }
  ssize_t (*real)(int, const void*, size_t) = dlsym(RTLD_NEXT, "write");
  return real(fd, data, size);
}

Term stall_open_run(Env e, Term* f, IoWork* w) {
  stall_fd = open("/dev/null", O_WRONLY);
  return stall_fd < 0 ? io_fail(e, (u32)errno, NULL)
    : io_done(e, io_hand(stall_fd));
}

static void __attribute__((constructor)) stall_use(void) {
  io_eff(BEND_CID_Stall_open, stall_open_run);
}
