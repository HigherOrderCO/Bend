// Mode 0 parks a sender of 99, 1 a timed receiver, 2 a receiver; then the
// nested IO fails, taking that waiter with it.
function host_inner(c, mode) {
  const park = (k) => mode === 0
    ? { $: CID(Chan.send), args: [c, 99], kont: k }
    : mode === 1 ? { $: CID(Chan.try_recv), args: [c, 5], kont: k }
    : { $: CID(Chan.recv), args: [c], kont: k };
  try {
    io_run(() => (cont) => ({
      $: CID(IO.spawn),
      args: [(k) => park(() => ({
        $: CID(IO.print), args: ["dead waiter resumed"], kont: k,
      }))],
      kont: () => ({ $: CID(IO.sleep), args: [1], kont: () => {
        throw "inner failed";
      } }),
    }));
    throw "nested IO did not fail";
  } catch (e) {
    if (e !== "inner failed") throw e;
  }
  return { $: CID(Unit) };
}

io_eff(CID(Host.inner), host_inner);
