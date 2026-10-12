let host_seen = false;

function host_start() {
  Promise.resolve().then(() => { host_seen = true; });
  return { $: CID(Unit) };
}

io_eff(CID(Host.start), host_start);
io_eff(CID(Host.seen), () => host_seen ? 1 : 0);
