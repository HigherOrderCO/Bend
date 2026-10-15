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
  return -1;
}

static void file_open_call(IoWork* w) {
  w->made = (intptr_t)io_sys_end(w, open(w->data, (int)w->word, 0644));
}

static Term file_open_pack(Env e, IoWork* w) {
  free(w->data);
  return io_res(e, w, io_hand(w->made));
}

Term file_open_run(Env e, Term* f, IoWork* w) {
  uint64_t mn = 0;
  w->data = io_cstr(e, f[0], &w->size);
  char* mode = io_cstr(e, f[1], &mn);
  int flags = io_nul(mode, mn) ? -1 : file_open_mode(mode);
  free(mode);
  w->word = (uint32_t)flags;
  if (io_nul(w->data, w->size) || flags < 0) {
    w->code = io_nul(w->data, w->size) ? EILSEQ : EINVAL;
    return file_open_pack(e, w);
  }
  return io_work(w, file_open_call, file_open_pack);
}

static void __attribute__((constructor)) file_open_use(void) {
  io_eff(CID(File.open), file_open_run);
}
