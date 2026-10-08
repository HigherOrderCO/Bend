// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.sleep BEND_CID_IO_sleep
// bend-ids end
// IO
// ==

// Parks until ms milliseconds from now.
function io_sleep(ms, k) {
  io_park_on(undefined, false, k, () => ({ $: BEND_CID_Unit }),
    performance.now() + Number(ms));
}

io_eff(BEND_CID_IO_sleep, io_sleep);
