// bend-ids 1
// CID #Wait BEND_CID_Wait
// CID #Unit BEND_CID_Unit
// CID :TCP.send BEND_CID_TCP_send
// CID :TCP.send_bytes BEND_CID_TCP_send_bytes
// CID :TCP.try_send BEND_CID_TCP_try_send
// CID :TCP.try_send_bytes BEND_CID_TCP_try_send_bytes
// bend-ids end
// TCP
// ===

// Sends what is left; a full socket (non-blocking, so EAGAIN) parks the
// computation until the socket is writable, and the loop resumes here.
function tcp_send_with(socket, b, k, make, at) {
  const sys = io_sys();
  const fd = socket;
  const again = sys.mac ? 35 : 11;
  const go = (off) => {
    while (off < b.length) {
      const part = b.subarray(off);
      const n = Number(sys.send(fd, sys.ptr(part), part.length, 0));
      if (n < 0) {
        const code = sys.errno();
        if (code !== again) {
          const fail = io_fail(code, make(part, part.length));
          return io_tup(socket, io_ready(at, fail));
        }
        if (io_late(at)) {
          const rest = make(part, part.length);
          return io_tup(socket, { $: BEND_CID_Wait, rest: rest });
        }
        io_park_on(fd, true, k, () => go(off), at);
        return undefined;
      }
      off += n;
    }
    return io_tup(socket, io_ready(at, io_done({ $: BEND_CID_Unit })));
  };
  return go(0);
}

// A value past 255 fails with EINVAL before any byte is sent, list kept.
function tcp_send_bytes_with(socket, data, k, at) {
  const b = io_unlist(data);
  if (b !== null) {
    return tcp_send_with(socket, b, k, io_list, at);
  }
  return io_tup(socket, io_ready(at, io_fail(22, data)));
}

function tcp_send(socket, data, k) {
  return tcp_send_with(socket, io_bytes(data), k, io_text);
}

function tcp_send_bytes(socket, data, k) {
  return tcp_send_bytes_with(socket, data, k);
}

// The try_ twins pass a deadline at: past it, a waiting send is Wait{rest}.
function tcp_try_send(socket, data, ms, k) {
  return tcp_send_with(socket, io_bytes(data), k, io_text, io_until(ms));
}

function tcp_try_send_bytes(socket, data, ms, k) {
  return tcp_send_bytes_with(socket, data, k, io_until(ms));
}

io_eff(BEND_CID_TCP_send, tcp_send);
io_eff(BEND_CID_TCP_send_bytes, tcp_send_bytes);
io_eff(BEND_CID_TCP_try_send, tcp_try_send);
io_eff(BEND_CID_TCP_try_send_bytes, tcp_try_send_bytes);
