// TCP
// ===

// The socket is non-blocking for life. The connect's outcome is w->code:
// EINPROGRESS parked the computation until the socket was writable, and
// then SO_ERROR says how it ended.
static Term tcp_connect_more(Env e, IoWork* w) {
  int       fd  = (int)w->made;
  int       err = (int)w->code;
  socklen_t len = sizeof(err);
  if (err == EINPROGRESS && getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &len)) {
    err = errno;
  }
  if (err != 0 && fd >= 0) {
    close(fd);
  }
  free(w->data);
  return err != 0 ? io_fail(e, (u32)err, NULL) : io_done(e, io_hand(fd));
}

Term tcp_connect_run(Env e, Term* f, IoWork* w) {
  struct sockaddr_in at;
  w->data = io_cstr(e, f[0], &w->size);
  int fd  = -1;
  errno   = EINVAL;
  if (!io_nul(w->data, w->size) && io_sys_addr(w->data, (u32)f[1], &at) == 0) {
    fd = socket(AF_INET, SOCK_STREAM, 0);
  }
  if (fd >= 0 && fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) < 0) {
    close(fd);
    fd = -1;
  }
  w->made = fd;
  io_sys_end(w, fd < 0 ? fd : connect(fd, (struct sockaddr*)&at, sizeof(at)));
  return w->code == EINPROGRESS
    ? io_wait_on(w, fd, POLLOUT, 0, tcp_connect_more) : tcp_connect_more(e, w);
}

static void __attribute__((constructor)) tcp_connect_use(void) {
  io_eff(CID(TCP.connect), tcp_connect_run);
}
