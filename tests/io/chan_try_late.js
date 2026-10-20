// Blocks the loop for ms milliseconds inside one effect.
function hold(ms) {
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
  return { $: CID(Unit) };
}

io_eff(CID(hold), hold);
