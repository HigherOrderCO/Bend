function host_inner(c) {
  try {
    io_run(() => (cont) => ({
      $: CID(IO.spawn),
      args: [(k) => ({ $: CID(Chan.recv), args: [c], kont: (x) => ({
        $: CID(IO.print), args: ["inner fiber got " + x.value], kont: k,
      }) })],
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
