// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :wall_clock_backwards BEND_CID_wall_clock_backwards
// bend-ids end
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
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_wall_clock_backwards, wall_clock_backwards);
