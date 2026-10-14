// TCP
// ===

// The request parks until the listener is readable, so a backlog never
// keeps the loop from its timers; an accept that still finds no connection
// (the listener is non-blocking) parks again. The accepted socket is
// non-blocking for life.
function tcp_accept_with(listener, k, at) {
  const sys = io_sys();
  const lfd = listener;
  const go = () => {
    const fd = sys.accept(lfd, null, null);
    if (fd < 0) {
      const code = sys.errno();
      if (code !== (sys.mac ? 35 : 11)) {
        return io_tup(listener, io_ready(at, io_fail(code)));
      }
      if (io_late(at)) {
        return io_tup(listener, { $: CID(Wait), rest: { $: CID(Unit) } });
      }
      io_park_on(lfd, false, k, go, at);
      return undefined;
    }
    if (sys.fcntl(fd, 4, sys.fcntl(fd, 3, 0) | (sys.mac ? 4 : 0x800)) < 0) {
      const code = sys.errno();
      sys.close(fd);
      return io_tup(listener, io_ready(at, io_fail(code)));
    }
    return io_tup(listener, io_ready(at, io_done(fd)));
  };
  io_park_on(lfd, false, k, go, at);
  return undefined;
}

function tcp_accept(listener, k) {
  return tcp_accept_with(listener, k);
}

// try_ passes a deadline at: past it, an accept that would wait answers Wait{}.
function tcp_try_accept(listener, ms, k) {
  return tcp_accept_with(listener, k, io_until(ms));
}

io_eff(CID(TCP.accept), tcp_accept);
io_eff(CID(TCP.try_accept), tcp_try_accept);
