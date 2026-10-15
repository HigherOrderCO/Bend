// One second backwards between opening the ring and its empty write.
const wall_now = Date.now;

function wall_clock_backwards(reset) {
  if (reset) {
    Date.now = wall_now;
  } else {
    let first = true;
    Date.now = () => {
      if (first) {
        first = false;
        return 1000;
      }
      return 0;
    };
  }
  return { $: CID(Unit) };
}

io_eff(CID(wall_clock_backwards), wall_clock_backwards);
