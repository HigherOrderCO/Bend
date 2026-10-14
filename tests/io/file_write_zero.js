// Every nonempty write to the fd Stall.open answers makes no progress.
function stall_open() {
  const fs = require("fs");
  const fd = fs.openSync("/dev/null", "w");
  const real = fs.writeSync;
  fs.writeSync = (f, ...a) =>
    f === fd && a[2] > 0 ? 0 : real.call(fs, f, ...a);
  return io_done(fd);
}

io_eff(CID(Stall.open), stall_open);
