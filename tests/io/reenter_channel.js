function host_channel(c, mode) {
  const code = io_run(() => (cont) => ({
    $: mode === 2 ? CID(Chan.recv) : mode === 3 ? CID(Chan.close) : CID(Chan.send),
    args: mode === 2 || mode === 3 ? [c] : [c, 7],
    kont: cont,
  }));
  if (code !== 0) throw "nested channel IO failed";
  return { $: CID(Unit) };
}

io_eff(CID(Host.channel), host_channel);
