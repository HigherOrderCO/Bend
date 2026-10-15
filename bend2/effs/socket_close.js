// Socket
// ======

function socket_close(socket) {
  io_sys().close(socket);
  return { $: CID(Unit) };
}

io_eff(CID(Socket.close), socket_close);
