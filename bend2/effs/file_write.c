// File
// ====

static void file_write_call(IoWork* w) {
  int fd = (int)w->hand;
  ssize_t n = 0;
  for (uint64_t at = 0; n >= 0 && at < w->size; at += (uint64_t)n) {
    n = write(fd, w->data + at, w->size - at);
  }
  io_sys_end(w, n);
}

static Term file_write_pack(Env e, IoWork* w) {
  Term r = io_res(e, w, term_pak(CID(Unit), 0));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

#ifdef CID(File.write)

Term file_write_run(Env e, Term* f, IoWork* w) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->data = io_cstr(e, f[1], &w->size);
  return io_work(w, file_write_call, file_write_pack);
}

static void __attribute__((constructor)) file_write_use(void) {
  io_eff(CID(File.write), file_write_run);
}

#endif

#ifdef CID(File.write_bytes)

// A value past 255 fails with EINVAL before any byte is written.
Term file_write_bytes_run(Env e, Term* f, IoWork* w) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->data = io_cbuf(e, f[1], &w->size, CID(Con));
  w->code = w->data == NULL ? EINVAL : 0;
  return w->code ? file_write_pack(e, w)
    : io_work(w, file_write_call, file_write_pack);
}

static void __attribute__((constructor)) file_write_bytes_use(void) {
  io_eff(CID(File.write_bytes), file_write_bytes_run);
}

#endif
