// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :Listener.close BEND_CID_Listener_close
// bend-ids end
// Listener
// ========

function listener_close(listener) {
  io_sys().close(listener);
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Listener_close, listener_close);
