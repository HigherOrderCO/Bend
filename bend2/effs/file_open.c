// File
// ====

static int file_open_mode(const char* mode) {
  if (strcmp(mode, "r") == 0) {
    return O_RDONLY;
  }
  if (strcmp(mode, "w") == 0) {
    return O_WRONLY | O_CREAT | O_TRUNC;
  }
  if (strcmp(mode, "a") == 0) {
    return O_WRONLY | O_CREAT | O_APPEND;
  }
  // "x" creates the file, and fails EEXIST if the path is there.
  if (strcmp(mode, "x") == 0) {
    return O_WRONLY | O_CREAT | O_EXCL;
  }
  return -1;
}

static void file_open_call(IoWork* w) {
  w->made = (intptr_t)io_sys_end(w, open(w->data, (int)w->word, 0644));
}

static Term file_open_pack(Env e, IoWork* w) {
  free(w->data);
  return io_res(e, w, io_hand(w->made));
}

// cons is CID(SCon) for a String path, opened as its UTF-8, or CID(Con)
// for a List, opened as its bytes (NULL past 255).
static Term file_open_start(Env e, Term* f, IoWork* w, u64 cons) {
  uint64_t mn = 0;
  w->data = io_cbuf(e, f[0], &w->size, cons);
  char* mode = io_cstr(e, f[1], &mn);
  int flags = io_nul(mode, mn) ? -1 : file_open_mode(mode);
  free(mode);
  w->word = (uint32_t)flags;
  w->code = w->data == NULL ? EINVAL : io_nul(w->data, w->size) ? EILSEQ
    : flags < 0 ? EINVAL : 0;
  if (w->code != 0) {
    return file_open_pack(e, w);
  }
  return io_work(w, file_open_call, file_open_pack);
}

#ifdef CID(File.open)

Term file_open_run(Env e, Term* f, IoWork* w) {
  return file_open_start(e, f, w, CID(SCon));
}

static void __attribute__((constructor)) file_open_use(void) {
  io_eff(CID(File.open), file_open_run);
}

#endif

#ifdef CID(File.open_bytes)

Term file_open_bytes_run(Env e, Term* f, IoWork* w) {
  return file_open_start(e, f, w, CID(Con));
}

static void __attribute__((constructor)) file_open_bytes_use(void) {
  io_eff(CID(File.open_bytes), file_open_bytes_run);
}

#endif
