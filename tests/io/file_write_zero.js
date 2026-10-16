// bend-ids 1
// CID :Stall.open BEND_CID_Stall_open
// bend-ids end
// Every nonempty write to the fd Stall.open answers makes no progress.
function stall_open() {
  const fs = require("fs");
  const fd = fs.openSync("/dev/null", "w");
  const real = fs.writeSync;
  fs.writeSync = (f, ...a) =>
    f === fd && a[2] > 0 ? 0 : real.call(fs, f, ...a);
  return io_done(fd);
}

io_eff(BEND_CID_Stall_open, stall_open);
