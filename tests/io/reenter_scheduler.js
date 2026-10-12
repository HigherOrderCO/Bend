function host_nested(mode, k) {
  const nested = mode === 0
    ? io_run(() => (cont) => ({
      $: CID(IO.sleep), args: [1], kont: () => ({
        $: CID(IO.print), args: ["inner"], kont: cont,
      }),
    }))
    : io_run(() => () => { throw "expected nested error"; });
  return io_park_promise(nested, k, (code) => {
    if (mode !== 0 || code !== 0) throw "nested IO did not finish correctly";
    return { $: CID(Unit) };
  }, (error) => {
    if (mode === 0 || error !== "expected nested error") throw error;
    return { $: CID(Unit) };
  });
}

io_eff(CID(Host.nested), host_nested);
