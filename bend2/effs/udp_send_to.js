// UDP
// ===

// A datagram goes whole or not at all; a full send buffer (non-blocking,
// so EAGAIN) parks the computation until the socket is writable. b is the
// datagram's bytes, data the value a failed or late send hands back.
function udp_send_to_with(socket, host, port, data, b, k, at) {
  const fail = (code) => io_tup(socket, io_ready(at, io_fail(code, data)));
  const sys = io_sys();
  const fd = socket;
  const to = io_addr(host, Number(port));
  if (to === null) {
    return fail(22);
  }
  const go = () => {
    const sent = sys.sendto(fd, b.length ? sys.ptr(b) : null, b.length, 0, sys.ptr(to), 16);
    if (Number(sent) < 0) {
      const code = sys.errno();
      if (code !== (sys.mac ? 35 : 11)) {
        return fail(code);
      }
      if (io_late(at)) {
        return io_tup(socket, { $: CID(Wait), rest: data });
      }
      io_park_on(fd, true, k, go, at);
      return undefined;
    }
    return io_tup(socket, io_ready(at, io_done({ $: CID(Unit) })));
  };
  return go();
}

// A value past 255 fails with EINVAL before the datagram is sent, list kept.
function udp_send_bytes_to_with(socket, host, port, data, k, at) {
  const b = io_unlist(data);
  if (b !== null) {
    return udp_send_to_with(socket, host, port, data, b, k, at);
  }
  return io_tup(socket, io_ready(at, io_fail(22, data)));
}

function udp_send_to(socket, host, port, data, k) {
  return udp_send_to_with(socket, host, port, data, io_bytes(data), k);
}

function udp_send_bytes_to(socket, host, port, data, k) {
  return udp_send_bytes_to_with(socket, host, port, data, k);
}

// The try_ twins pass a deadline at: past it, a send that would wait is
// Wait{data}.
function udp_try_send_to(socket, host, port, data, ms, k) {
  return udp_send_to_with(socket, host, port, data, io_bytes(data), k,
    io_until(ms));
}

function udp_try_send_bytes_to(socket, host, port, data, ms, k) {
  return udp_send_bytes_to_with(socket, host, port, data, k, io_until(ms));
}

io_eff(CID(UDP.send_to), udp_send_to);
io_eff(CID(UDP.send_bytes_to), udp_send_bytes_to);
io_eff(CID(UDP.try_send_to), udp_try_send_to);
io_eff(CID(UDP.try_send_bytes_to), udp_try_send_bytes_to);
