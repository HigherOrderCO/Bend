let value = 7;
io_eff(CID(now), () => ({ $: CID(Instant), value: ++value }));
