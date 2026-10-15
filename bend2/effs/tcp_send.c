// TCP
// ===

// Sends what is left; a full socket (non-blocking, so EAGAIN) parks the
// computation until the socket is writable, and the loop resumes here.
static Term tcp_send_with(Env e, IoWork* w, IoPack more,
  Term (*make)(Env, const char*, u64)) {
  int fd = (int)w->hand;
  w->code = 0;
  while (w->code == 0 && (u64)w->made < w->size) {
    w->made += io_sys_end(w, send(fd, w->data + w->made,
      w->size - (u64)w->made, 0));
    if (io_again(w)) {
      return io_wait_on(w, fd, POLLOUT, w->time, more);
    }
  }
  u64  left = w->size - (u64)w->made;
  Term r    = io_poll_end(e, w, make(e, w->data + w->made, left), w->code == 0
    ? io_done(e, term_pak(CID(Unit), 0)) : io_box(e, CID(Fail),
      io_tup(e, io_err(e, w->code, NULL), make(e, w->data + w->made, left))));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

static Term tcp_send_more(Env e, IoWork* w) {
  return tcp_send_with(e, w, tcp_send_more, io_str);
}

// at is the try_ deadline (past it, Wait{rest}), 0 for the blocking twins.
static Term tcp_send_start(Env e, Term* f, IoWork* w, u64 at) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->data = io_cstr(e, f[1], &w->size);
  w->made = 0;
  w->code = 0;
  w->time = at;
  return tcp_send_more(e, w);
}

#ifdef CID(Con)

static Term tcp_send_bytes_more(Env e, IoWork* w) {
  return tcp_send_with(e, w, tcp_send_bytes_more, io_list);
}

// A value past 255 fails with EINVAL before any byte is sent, list kept.
static Term tcp_send_bytes_start(Env e, Term* f, IoWork* w, u64 at) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  for (Term s = f[1]; term_aux(s) == CID(Con);) {
    u64 l = term_peek(e.mem, s);
    if (e.mem[l] > 255) {
      w->code = EINVAL;
      w->time = at;
      return io_tup(e, f[0], io_poll_end(e, w, f[1], io_box(e, CID(Fail),
        io_tup(e, io_err(e, EINVAL, NULL), f[1]))));
    }
    s = e.mem[l + 1];
  }
  w->data = io_cbuf(e, f[1], &w->size, CID(Con));
  w->made = 0;
  w->code = 0;
  w->time = at;
  return tcp_send_bytes_more(e, w);
}

#endif

#ifdef CID(TCP.send)

Term tcp_send_run(Env e, Term* f, IoWork* w) {
  return tcp_send_start(e, f, w, 0);
}

static void __attribute__((constructor)) tcp_send_use(void) {
  io_eff(CID(TCP.send), tcp_send_run);
}

#endif

#ifdef CID(TCP.send_bytes)

Term tcp_send_bytes_run(Env e, Term* f, IoWork* w) {
  return tcp_send_bytes_start(e, f, w, 0);
}

static void __attribute__((constructor)) tcp_send_bytes_use(void) {
  io_eff(CID(TCP.send_bytes), tcp_send_bytes_run);
}

#endif

#ifdef CID(TCP.try_send)

Term tcp_try_send_run(Env e, Term* f, IoWork* w) {
  return tcp_send_start(e, f, w, io_until(f[2]));
}

static void __attribute__((constructor)) tcp_try_send_use(void) {
  io_eff(CID(TCP.try_send), tcp_try_send_run);
}

#endif

#ifdef CID(TCP.try_send_bytes)

Term tcp_try_send_bytes_run(Env e, Term* f, IoWork* w) {
  return tcp_send_bytes_start(e, f, w, io_until(f[2]));
}

static void __attribute__((constructor)) tcp_try_send_bytes_use(void) {
  io_eff(CID(TCP.try_send_bytes), tcp_try_send_bytes_run);
}

#endif
