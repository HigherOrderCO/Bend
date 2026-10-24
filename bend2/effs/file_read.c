// File
// ====

// read(2) answers what is there, not what was asked: a pipe, a terminal
// and a short read all stop before max. Fill the buffer or reach EOF, so
// a caller that asked for n bytes gets n unless the file ends first.
static void file_read_call(IoWork* w) {
  int fd = (int)w->hand;
  u64 got = 0;
  while (got < w->word) {
    ssize_t n = read(fd, w->data + got, w->word - got);
    if (n < 0) {
      if (errno == EINTR) {
        continue;
      }
      if (got == 0) {
        io_sys_end(w, n);
        return;
      }
      break;
    }
    if (n == 0) {
      break;
    }
    got += (u64)n;
  }
  w->size = got;
}

static Term file_read_start(Term file, u64 max, IoWork* w,
  IoCall call, IoPack pack) {
  w->hand = (intptr_t)io_hand_v(file);
  w->word = max < INT32_MAX ? max : INT32_MAX;
  w->data = io_mem(malloc(w->word + 1));
  return io_work(w, call, pack);
}

#ifdef CID(File.read)

static Term file_read_pack(Env e, IoWork* w) {
  Term r = io_res(e, w, io_str(e, w->data, w->size));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

Term file_read_run(Env e, Term* f, IoWork* w) {
  return file_read_start(f[0], f[1], w, file_read_call, file_read_pack);
}

static void __attribute__((constructor)) file_read_use(void) {
  io_eff(CID(File.read), file_read_run);
}

#endif

#if defined(CID(File.read_bytes)) || defined(CID(File.read_at))

static Term file_read_bytes_pack(Env e, IoWork* w) {
  Term r = io_res(e, w, io_list(e, w->data, w->size));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

#endif

#ifdef CID(File.read_bytes)

Term file_read_bytes_run(Env e, Term* f, IoWork* w) {
  return file_read_start(f[0], f[1], w, file_read_call, file_read_bytes_pack);
}

static void __attribute__((constructor)) file_read_bytes_use(void) {
  io_eff(CID(File.read_bytes), file_read_bytes_run);
}

#endif

#ifdef CID(File.read_at)

// The bytes at an offset, as file_read_bytes gives them; the position of
// the file does not move.
static void file_read_at_call(IoWork* w) {
  int fd = (int)w->hand;
  u64 got = 0;
  while (got < w->word) {
    ssize_t n = pread(fd, w->data + got, w->word - got,
      (off_t)w->made + (off_t)got);
    if (n < 0) {
      if (errno == EINTR) {
        continue;
      }
      if (got == 0) {
        io_sys_end(w, n);
        return;
      }
      break;
    }
    if (n == 0) {
      break;
    }
    got += (u64)n;
  }
  w->size = got;
}

Term file_read_at_run(Env e, Term* f, IoWork* w) {
  w->made = (intptr_t)f[1];
  return file_read_start(f[0], f[2], w, file_read_at_call, file_read_bytes_pack);
}

static void __attribute__((constructor)) file_read_at_use(void) {
  io_eff(CID(File.read_at), file_read_at_run);
}

#endif
