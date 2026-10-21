// bend-ids 1
// CID :Chan.send BEND_CID_Chan_send
// CID :Chan.try_recv BEND_CID_Chan_try_recv
// CID :Chan.recv BEND_CID_Chan_recv
// CID :IO.spawn BEND_CID_IO_spawn
// CID :IO.print BEND_CID_IO_print
// CID :IO.sleep BEND_CID_IO_sleep
// CID #Unit BEND_CID_Unit
// CID :Host.inner BEND_CID_Host_inner
// bend-ids end
// Mode 0 parks a sender of 99, 1 a timed receiver, 2 a receiver; then the
// nested IO fails, taking that waiter with it.
function host_inner(c, mode) {
  const park = (k) => mode === 0
    ? { $: BEND_CID_Chan_send, args: [c, 99], kont: k }
    : mode === 1 ? { $: BEND_CID_Chan_try_recv, args: [c, 5], kont: k }
    : { $: BEND_CID_Chan_recv, args: [c], kont: k };
  try {
    io_run(() => (cont) => ({
      $: BEND_CID_IO_spawn,
      args: [(k) => park(() => ({
        $: BEND_CID_IO_print, args: ["dead waiter resumed"], kont: k,
      }))],
      kont: () => ({ $: BEND_CID_IO_sleep, args: [1], kont: () => {
        throw "inner failed";
      } }),
    }));
    throw "nested IO did not fail";
  } catch (e) {
    if (e !== "inner failed") throw e;
  }
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Host_inner, host_inner);
