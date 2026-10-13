let value = 70;
io_eff(CID(now), () => ({ $: CID(Instant), value: ++value }));
