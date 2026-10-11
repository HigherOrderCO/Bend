// TCP
// ===

// The request parks until the listener is readable, so a backlog never
// keeps the loop from its timers; an accept that still finds no connection
// (the listener is non-blocking) parks again. The accepted socket is
// non-blocking for life.
static Term tcp_accept_more(Env e, IoWork* w) {
  int fd  = (int)w->hand;
  int got = accept(fd, NULL, NULL);
  if (got >= 0 && fcntl(got, F_SETFL, fcntl(got, F_GETFL) | O_NONBLOCK) < 0) {
    close(got);
    got = -1;
  }
  io_sys_end(w, got);
  if (io_again(w)) {
    return io_wait_on(w, fd, POLLIN, w->time, tcp_accept_more);
  }
  return io_tup(e, io_hand(fd), io_poll_end(e, w, term_pak(CID(Unit), 0),
    io_res(e, w, io_hand(got))));
}

#ifdef CID(TCP.accept)

Term tcp_accept_run(Env e, Term* f, IoWork* w) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  return io_wait_on(w, (int)w->hand, POLLIN, 0, tcp_accept_more);
}

static void __attribute__((constructor)) tcp_accept_use(void) {
  io_eff(CID(TCP.accept), tcp_accept_run);
}

#endif

#ifdef CID(TCP.try_accept)

// w->time is the deadline, past which tcp_accept_more answers Wait{}.
Term tcp_try_accept_run(Env e, Term* f, IoWork* w) {
  w->hand = (intptr_t)io_hand_v(f[0]);
  return io_wait_on(w, (int)w->hand, POLLIN, io_until(f[1]),
    tcp_accept_more);
}

static void __attribute__((constructor)) tcp_try_accept_use(void) {
  io_eff(CID(TCP.try_accept), tcp_try_accept_run);
}

#endif
