// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :Socket.close BEND_CID_Socket_close
// bend-ids end
// Socket
// ======

function socket_close(socket) {
  io_sys().close(socket);
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Socket_close, socket_close);
