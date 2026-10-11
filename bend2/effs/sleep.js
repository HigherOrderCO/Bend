// IO
// ==

// Parks until ms milliseconds from now.
function io_sleep(ms, k) {
  io_park_on(undefined, false, k, () => ({ $: CID(Unit) }),
    performance.now() + Number(ms));
}

io_eff(CID(IO.sleep), io_sleep);
