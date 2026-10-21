// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :hold BEND_CID_hold
// bend-ids end
// Blocks the loop for ms milliseconds inside one effect.
function hold(ms) {
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_hold, hold);
