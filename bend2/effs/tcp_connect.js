// TCP
// ===

// The socket is non-blocking for life: EINPROGRESS parks the computation
// until the socket is writable, and then SO_ERROR says how the connect ended.
function tcp_connect(host, port, k) {
  const sys = io_sys();
  const at = io_addr(host, Number(port));
  if (at === null) {
    return io_fail(22);
  }
  const fd = sys.socket(2, 1, 0);
  if (fd < 0) {
    return io_fail(sys.errno());
  }
  const end = (code) => {
    if (code !== 0) {
      sys.close(fd);
      return io_fail(code);
    }
    return io_done(fd);
  };
  const error = () => {
    const v = new Int32Array([0]);
    const l = new Uint32Array([4]);
    return sys.getsockopt(fd, sys.mac ? 0xffff : 1, sys.mac ? 0x1007 : 4,
      sys.ptr(v), sys.ptr(l)) < 0 ? sys.errno() : v[0];
  };
  const set = sys.fcntl(fd, 4, sys.fcntl(fd, 3, 0) | (sys.mac ? 4 : 0x800));
  const ok = set >= 0 && sys.connect(fd, sys.ptr(at), 16) >= 0;
  const code = ok ? 0 : sys.errno();
  if (code !== (sys.mac ? 36 : 115)) {
    return end(code);
  }
  io_park_on(fd, true, k, () => end(error()));
  return undefined;
}

io_eff(CID(TCP.connect), tcp_connect);
