// UDP
// ===

Term udp_bind_run(Env e, Term* f, IoWork* w) {
  struct sockaddr_in at;
  u64 len;
  char* host = io_cstr(e, f[0], &len);
  int valid = !io_nul(host, len) && io_sys_addr(host, (u32)f[1], &at) == 0;
  free(host);
  if (!valid) {
    return io_fail(e, EINVAL, NULL);
  }
  int fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (fd < 0) {
    return io_fail(e, (uint32_t)errno, NULL);
  }
  if (bind(fd, (struct sockaddr*)&at, sizeof(at)) < 0
    || fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) < 0) {
    uint32_t code = (uint32_t)errno;
    close(fd);
    return io_fail(e, code, NULL);
  }
  return io_done(e, io_hand(fd));
}

static void __attribute__((constructor)) udp_bind_use(void) {
  io_eff(CID(UDP.bind), udp_bind_run);
}
