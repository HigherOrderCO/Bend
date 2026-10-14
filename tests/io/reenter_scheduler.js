function host_nested(mode, k) {
  if (mode === 0) {
    const code = io_run(() => (cont) => ({
      $: CID(IO.print), args: ["inner"], kont: cont,
    }));
    if (code !== 0) throw "nested IO failed";
  } else {
    try {
      io_run(() => () => { throw "expected nested error"; });
      throw "nested IO did not fail";
    } catch (e) {
      if (e !== "expected nested error") throw e;
    }
  }
  return { $: CID(Unit) };
}

io_eff(CID(Host.nested), host_nested);
