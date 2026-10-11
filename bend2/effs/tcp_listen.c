// TCP
// ===

Term tcp_listen_run(Env e, Term* f, IoWork* w) {
  struct sockaddr_in at;
  u64 len;
  char* host = io_cstr(e, f[0], &len);
  int valid = !io_nul(host, len) && io_sys_addr(host, (u32)f[1], &at) == 0;
  free(host);
  if (!valid) {
    return io_fail(e, EINVAL, NULL);
  }
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) {
    return io_fail(e, (uint32_t)errno, NULL);
  }
  int one = 1;
  setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
  int bound = bind(fd, (struct sockaddr*)&at, sizeof(at));
  if (bound < 0 || listen(fd, 512) < 0
    || fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) < 0) {
    uint32_t code = (uint32_t)errno;
    close(fd);
    return io_fail(e, code, NULL);
  }
  return io_done(e, io_hand(fd));
}

static void __attribute__((constructor)) tcp_listen_use(void) {
  io_eff(CID(TCP.listen), tcp_listen_run);
}
