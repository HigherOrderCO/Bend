// The cases run only in the Linux C lane.
io_eff(CID(AudioTest.count), () => 0);
io_eff(CID(AudioTest.start), () => ({ $: CID(Unit) }));
io_eff(CID(AudioTest.wait), () => ({ $: CID(Unit) }));
io_eff(CID(AudioTest.closed), () => ({ $: CID(Unit) }));
