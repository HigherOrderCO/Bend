// UDP
// ===

// A datagram goes whole or not at all; a full send buffer (non-blocking,
// so EAGAIN) parks the computation on more until the socket is writable.
// make hands the datagram back as it came: a String (io_str) or a List of
// bytes (io_list).
static Term udp_send_to_with(Env e, IoWork* w, IoPack more,
  Term (*make)(Env, const char*, u64)) {
  struct sockaddr_in at;
  int     fd = (int)w->hand;
  ssize_t n  = -1;
  errno      = EINVAL;
  if (io_sys_addr(w->text, (u32)w->made, &at) == 0) {
    n = sendto(fd, w->data, w->size, 0, (struct sockaddr*)&at, sizeof(at));
  }
  io_sys_end(w, n);
  if (io_again(w)) {
    return io_wait_on(w, fd, POLLOUT, w->time, more);
  }
  Term r = io_poll_end(e, w, make(e, w->data, w->size), w->code == 0
    ? io_done(e, term_pak(CID(Unit), 0)) : io_box(e, CID(Fail),
      io_tup(e, io_err(e, w->code, NULL), make(e, w->data, w->size))));
  free(w->text);
  free(w->data);
  return io_tup(e, io_hand(w->hand), r);
}

static Term udp_send_to_more(Env e, IoWork* w) {
  return udp_send_to_with(e, w, udp_send_to_more, io_str);
}

// at is the try_ deadline (past it, Wait{data}), 0 for the blocking twins.
// A host with a NUL in it reads as empty: no address, so EINVAL.
static void udp_send_to_at(Env e, Term* f, IoWork* w, u64 at) {
  uint64_t hn = 0;
  w->hand = (intptr_t)io_hand_v(f[0]);
  w->text = io_cstr(e, f[1], &hn);
  w->made = (intptr_t)f[2];
  w->time = at;
  if (io_nul(w->text, hn)) {
    w->text[0] = 0;
  }
}

static Term udp_send_to_start(Env e, Term* f, IoWork* w, u64 at) {
  udp_send_to_at(e, f, w, at);
  w->data = io_cstr(e, f[3], &w->size);
  return udp_send_to_more(e, w);
}

#ifdef CID(Con)

static Term udp_send_bytes_to_more(Env e, IoWork* w) {
  return udp_send_to_with(e, w, udp_send_bytes_to_more, io_list);
}

// A value past 255 fails with EINVAL before the datagram is sent, list kept.
static Term udp_send_bytes_to_start(Env e, Term* f, IoWork* w, u64 at) {
  udp_send_to_at(e, f, w, at);
  for (Term s = f[3]; term_aux(s) == CID(Con);) {
    u64 l = term_peek(e.mem, s);
    if (e.mem[l] > 255) {
      free(w->text);
      w->code = EINVAL;
      return io_tup(e, f[0], io_poll_end(e, w, f[3], io_box(e, CID(Fail),
        io_tup(e, io_err(e, EINVAL, NULL), f[3]))));
    }
    s = e.mem[l + 1];
  }
  w->data = io_cbuf(e, f[3], &w->size, CID(Con));
  return udp_send_bytes_to_more(e, w);
}

#endif

#ifdef CID(UDP.send_to)

Term udp_send_to_run(Env e, Term* f, IoWork* w) {
  return udp_send_to_start(e, f, w, 0);
}

static void __attribute__((constructor)) udp_send_to_use(void) {
  io_eff(CID(UDP.send_to), udp_send_to_run);
}

#endif

#ifdef CID(UDP.try_send_to)

Term udp_try_send_to_run(Env e, Term* f, IoWork* w) {
  return udp_send_to_start(e, f, w, io_until(f[4]));
}

static void __attribute__((constructor)) udp_try_send_to_use(void) {
  io_eff(CID(UDP.try_send_to), udp_try_send_to_run);
}

#endif

#ifdef CID(UDP.send_bytes_to)

Term udp_send_bytes_to_run(Env e, Term* f, IoWork* w) {
  return udp_send_bytes_to_start(e, f, w, 0);
}

static void __attribute__((constructor)) udp_send_bytes_to_use(void) {
  io_eff(CID(UDP.send_bytes_to), udp_send_bytes_to_run);
}

#endif

#ifdef CID(UDP.try_send_bytes_to)

Term udp_try_send_bytes_to_run(Env e, Term* f, IoWork* w) {
  return udp_send_bytes_to_start(e, f, w, io_until(f[4]));
}

static void __attribute__((constructor)) udp_try_send_bytes_to_use(void) {
  io_eff(CID(UDP.try_send_bytes_to), udp_try_send_bytes_to_run);
}

#endif
