// UDP
// ===

// The request parks until the socket is readable, so a backlog never keeps
// the loop from its timers; a recv that still finds no datagram (the socket
// is non-blocking) parks again on more. The datagram, read makes a String
// (io_str) or a List of bytes (io_list).
static Term udp_recv_from_with(Env e, IoWork* w, IoPack more,
  Term (*read)(Env, const char*, u64)) {
  struct sockaddr_in at = { 0 };
  socklen_t alen = sizeof(at);
  char      host[16];
  int       fd = (int)w->hand;
  w->size = io_sys_end(w, recvfrom(fd, w->data, (size_t)w->made, 0,
    (struct sockaddr*)&at, &alen));
  if (io_again(w)) {
    return io_wait_on(w, fd, POLLIN, w->time, more);
  }
  inet_ntop(AF_INET, &at.sin_addr, host, 16);
  Term r = io_poll_end(e, w, term_pak(CID(Unit), 0), io_res(e, w,
    io_tup(e, io_str(e, host, strlen(host)),
      io_tup(e, ntohs(at.sin_port), read(e, w->data, w->size)))));
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

// at is the try_ deadline (past it, Wait{}), 0 for the blocking twins.
static Term udp_recv_from_start(Env e, Term* f, IoWork* w, IoPack more, u64 at) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->time = at;
  if (f[1] == 0) {
    w->code = EINVAL;
    return io_tup(e, io_hand(w->hand), io_poll_end(e, w,
      term_pak(CID(Unit), 0), io_fail(e, EINVAL, NULL)));
  }
  w->made = f[1] < INT32_MAX ? (intptr_t)f[1] : INT32_MAX;
  w->data = io_mem(malloc((size_t)w->made + 1));
  return io_wait_on(w, (int)w->hand, POLLIN, at, more);
}

static Term udp_recv_from_more(Env e, IoWork* w) {
  return udp_recv_from_with(e, w, udp_recv_from_more, io_str);
}

#ifdef CID(Con)

static Term udp_recv_bytes_from_more(Env e, IoWork* w) {
  return udp_recv_from_with(e, w, udp_recv_bytes_from_more, io_list);
}

#endif

#ifdef CID(UDP.recv_from)

Term udp_recv_from_run(Env e, Term* f, IoWork* w) {
  return udp_recv_from_start(e, f, w, udp_recv_from_more, 0);
}

static void __attribute__((constructor)) udp_recv_from_use(void) {
  io_eff(CID(UDP.recv_from), udp_recv_from_run);
}

#endif

#ifdef CID(UDP.try_recv_from)

Term udp_try_recv_from_run(Env e, Term* f, IoWork* w) {
  return udp_recv_from_start(e, f, w, udp_recv_from_more, io_until(f[2]));
}

static void __attribute__((constructor)) udp_try_recv_from_use(void) {
  io_eff(CID(UDP.try_recv_from), udp_try_recv_from_run);
}

#endif

#ifdef CID(UDP.recv_bytes_from)

Term udp_recv_bytes_from_run(Env e, Term* f, IoWork* w) {
  return udp_recv_from_start(e, f, w, udp_recv_bytes_from_more, 0);
}

static void __attribute__((constructor)) udp_recv_bytes_from_use(void) {
  io_eff(CID(UDP.recv_bytes_from), udp_recv_bytes_from_run);
}

#endif

#ifdef CID(UDP.try_recv_bytes_from)

Term udp_try_recv_bytes_from_run(Env e, Term* f, IoWork* w) {
  return udp_recv_from_start(e, f, w, udp_recv_bytes_from_more,
    io_until(f[2]));
}

static void __attribute__((constructor)) udp_try_recv_bytes_from_use(void) {
  io_eff(CID(UDP.try_recv_bytes_from), udp_try_recv_bytes_from_run);
}

#endif
