// IO
// ==

// JS has one thread: IO.eval applies f at once.
io_eff(CID(IO.eval), (f) => f({ $: CID(Unit) }));
