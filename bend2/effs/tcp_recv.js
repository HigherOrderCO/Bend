// TCP
// ===

// A recv that finds nothing (the socket is non-blocking) parks until the
// socket is readable. What it finds, read makes a String (io_text) or a
// List of bytes (io_list).
function tcp_recv_with(socket, max, k, read, at) {
  if (Number(max) === 0) {
    return io_tup(socket, io_ready(at, io_fail(22)));
  }
  const sys = io_sys();
  const fd = socket;
  const b = new Uint8Array(Number(max));
  const again = sys.mac ? 35 : 11;
  const go = () => {
    const n = Number(sys.recv(fd, sys.ptr(b), Number(max), 0));
    if (n < 0) {
      const code = sys.errno();
      if (code !== again) {
        return io_tup(socket, io_ready(at, io_fail(code)));
      }
      if (io_late(at)) {
        return io_tup(socket, { $: CID(Wait), rest: { $: CID(Unit) } });
      }
      io_park_on(fd, false, k, go, at);
      return undefined;
    }
    const got = n === 0 ? { $: CID(None) }
      : { $: CID(Some), value: read(b, n) };
    return io_tup(socket, io_ready(at, io_done(got)));
  };
  return go();
}

function tcp_recv(socket, max, k) {
  return tcp_recv_with(socket, max, k, io_text);
}

function tcp_recv_bytes(socket, max, k) {
  return tcp_recv_with(socket, max, k, io_list);
}

// The try_ twins pass a deadline at: past it, a recv that would wait is Wait{}.
function tcp_try_recv(socket, max, ms, k) {
  return tcp_recv_with(socket, max, k, io_text, io_until(ms));
}

function tcp_try_recv_bytes(socket, max, ms, k) {
  return tcp_recv_with(socket, max, k, io_list, io_until(ms));
}

io_eff(CID(TCP.recv), tcp_recv);
io_eff(CID(TCP.recv_bytes), tcp_recv_bytes);
io_eff(CID(TCP.try_recv), tcp_try_recv);
io_eff(CID(TCP.try_recv_bytes), tcp_try_recv_bytes);
