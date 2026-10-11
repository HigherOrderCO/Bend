// The C test exercises the Linux native runtime. Other lanes skip it.
io_eff(CID(AudioTest.mode), () => 0);
io_eff(CID(AudioTest.prepare), () => ({ $: CID(Unit) }));
io_eff(CID(AudioTest.wait), () => ({ $: CID(Unit) }));
io_eff(CID(AudioTest.closed), () => ({ $: CID(Unit) }));
