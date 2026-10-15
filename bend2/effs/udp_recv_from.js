// UDP
// ===

// The request parks until the socket is readable, so a backlog never keeps
// the loop from its timers; a recv that still finds no datagram (the socket
// is non-blocking) parks again. The datagram, read makes a String (io_text)
// or a List of bytes (io_list).
function udp_recv_from_with(socket, max, k, read, at) {
  if (Number(max) === 0) {
    return io_tup(socket, io_ready(at, io_fail(22)));
  }
  const sys = io_sys();
  const fd = socket;
  const b = new Uint8Array(Number(max));
  const peer = new Uint8Array(16);
  const len = new Uint32Array([16]);
  const go = () => {
    const got = sys.recvfrom(fd, sys.ptr(b), Number(max), 0, sys.ptr(peer),
      sys.ptr(len));
    const n = Number(got);
    if (n < 0) {
      const code = sys.errno();
      if (code !== (sys.mac ? 35 : 11)) {
        return io_tup(socket, io_ready(at, io_fail(code)));
      }
      if (io_late(at)) {
        return io_tup(socket, { $: CID(Wait), rest: { $: CID(Unit) } });
      }
      io_park_on(fd, false, k, go, at);
      return undefined;
    }
    const host = peer[4] + "." + peer[5] + "." + peer[6] + "." + peer[7];
    const port = (peer[2] << 8) | peer[3];
    const dgram = io_tup(host, port, read(b, n));
    return io_tup(socket, io_ready(at, io_done(dgram)));
  };
  io_park_on(fd, false, k, go, at);
  return undefined;
}

function udp_recv_from(socket, max, k) {
  return udp_recv_from_with(socket, max, k, io_text);
}

function udp_recv_bytes_from(socket, max, k) {
  return udp_recv_from_with(socket, max, k, io_list);
}

// The try_ twins pass a deadline at: past it, a recv that would wait
// answers Wait{}.
function udp_try_recv_from(socket, max, ms, k) {
  return udp_recv_from_with(socket, max, k, io_text, io_until(ms));
}

function udp_try_recv_bytes_from(socket, max, ms, k) {
  return udp_recv_from_with(socket, max, k, io_list, io_until(ms));
}

io_eff(CID(UDP.recv_from), udp_recv_from);
io_eff(CID(UDP.recv_bytes_from), udp_recv_bytes_from);
io_eff(CID(UDP.try_recv_from), udp_try_recv_from);
io_eff(CID(UDP.try_recv_bytes_from), udp_try_recv_bytes_from);
