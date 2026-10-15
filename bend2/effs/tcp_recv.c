// TCP
// ===

// A recv that finds nothing (the socket is non-blocking) parks on more
// until the socket is readable. What it finds, read makes a String (io_str)
// or a List of bytes (io_list).
static Term tcp_recv_with(Env e, IoWork* w, IoPack more,
  Term (*read)(Env, const char*, u64)) {
  int fd  = (int)w->hand;
  w->size = io_sys_end(w, recv(fd, w->data, (size_t)w->made, 0));
  if (io_again(w)) {
    return io_wait_on(w, fd, POLLIN, w->time, more);
  }
  Term r = io_poll_end(e, w, term_pak(CID(Unit), 0), io_res(e, w,
    w->size == 0 ? term_pak(CID(None), 0)
      : io_box(e, CID(Some), read(e, w->data, w->size))));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

// at is the try_ deadline (past it, Wait{}), 0 for the blocking twins.
static Term tcp_recv_start(Env e, Term* f, IoWork* w, IoPack more, u64 at) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->time = at;
  if (f[1] == 0) {
    w->code = EINVAL;
    return io_tup(e, io_hand(w->hand), io_poll_end(e, w,
      term_pak(CID(Unit), 0), io_fail(e, EINVAL, NULL)));
  }
  w->made = f[1] < INT32_MAX ? (intptr_t)f[1] : INT32_MAX;
  w->data = io_mem(malloc((size_t)w->made));
  return more(e, w);
}

static Term tcp_recv_more(Env e, IoWork* w) {
  return tcp_recv_with(e, w, tcp_recv_more, io_str);
}

#ifdef CID(Con)

static Term tcp_recv_bytes_more(Env e, IoWork* w) {
  return tcp_recv_with(e, w, tcp_recv_bytes_more, io_list);
}

#endif

#ifdef CID(TCP.recv)

Term tcp_recv_run(Env e, Term* f, IoWork* w) {
  return tcp_recv_start(e, f, w, tcp_recv_more, 0);
}

static void __attribute__((constructor)) tcp_recv_use(void) {
  io_eff(CID(TCP.recv), tcp_recv_run);
}

#endif

#ifdef CID(TCP.recv_bytes)

Term tcp_recv_bytes_run(Env e, Term* f, IoWork* w) {
  return tcp_recv_start(e, f, w, tcp_recv_bytes_more, 0);
}

static void __attribute__((constructor)) tcp_recv_bytes_use(void) {
  io_eff(CID(TCP.recv_bytes), tcp_recv_bytes_run);
}

#endif

#ifdef CID(TCP.try_recv)

Term tcp_try_recv_run(Env e, Term* f, IoWork* w) {
  return tcp_recv_start(e, f, w, tcp_recv_more, io_until(f[2]));
}

static void __attribute__((constructor)) tcp_try_recv_use(void) {
  io_eff(CID(TCP.try_recv), tcp_try_recv_run);
}

#endif

#ifdef CID(TCP.try_recv_bytes)

Term tcp_try_recv_bytes_run(Env e, Term* f, IoWork* w) {
  return tcp_recv_start(e, f, w, tcp_recv_bytes_more, io_until(f[2]));
}

static void __attribute__((constructor)) tcp_try_recv_bytes_use(void) {
  io_eff(CID(TCP.try_recv_bytes), tcp_try_recv_bytes_run);
}

#endif
