// A child bun gets a pipe whose read end is closed as fd and writes a
// line there with the runtime's own io_out, as IO.print does. The
// answer is the child's status as a shell shows it: 128 plus the signal
// that ended it, else its exit code.
function epipe_write(fd, k) {
  const ffi = require("bun:ffi");
  const sys = io_sys();
  const lib = ffi.dlopen(sys.mac ? "libSystem.dylib" : "libc.so.6", {
    pipe: { args: ["ptr"], returns: "i32" },
  }).symbols;
  const p = new Int32Array(2);
  lib.pipe(sys.ptr(p));
  sys.close(p[0]);
  const stdio = ["ignore", "inherit", "inherit"];
  stdio[fd] = p[1];
  const r = Bun.spawnSync([process.execPath, "-e",
    `${io_out}\nio_out(${fd}, Buffer.from("x\\n"));`], { stdio });
  sys.close(p[1]);
  return r.exitCode ?? 128 + require("os").constants.signals[r.signalCode];
}

io_eff(CID(Epipe.write), epipe_write);
