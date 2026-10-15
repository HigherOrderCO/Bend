// JS has one thread and no pool to hold.
io_eff(CID(Hold.on), () => ({ $: CID(Unit) }));
io_eff(CID(Hold.off), () => ({ $: CID(Unit) }));
