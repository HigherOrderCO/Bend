// Listener
// ========

function listener_close(listener) {
  io_sys().close(listener);
  return { $: CID(Unit) };
}

io_eff(CID(Listener.close), listener_close);
