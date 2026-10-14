// File
// ====

#include <sys/stat.h>

// The size in bytes, as the host reports it; a file past 4 GiB fails
// with EOVERFLOW.
static void file_size_call(IoWork* w) {
  struct stat st;
  int n = fstat((int)w->hand, &st);
  io_sys_end(w, n);
  if (n == 0) {
    w->code = st.st_size > (off_t)UINT32_MAX ? EOVERFLOW : 0;
    w->word = (u32)st.st_size;
  }
}

static Term file_size_pack(Env e, IoWork* w) {
  return io_tup(e, io_hand(w->hand), io_res(e, w, w->word));
}

Term file_size_run(Env e, Term* f, IoWork* w) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  return io_work(w, file_size_call, file_size_pack);
}

static void __attribute__((constructor)) file_size_use(void) {
  io_eff(CID(File.size), file_size_run);
}
