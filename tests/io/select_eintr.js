// A listener makes Bun install a handler for SIGALRM (ignored by
// default, so a run that never got the handler is not killed); an
// interval timer has the kernel send it ms later, whatever the loop is
// doing then. A child process cannot promise that: a kill spawned here
// lands in about half a millisecond on Linux, most often before the
// loop reaches select, and the test then exercises nothing. Then the
// computation parks on the read end of a pipe whose write end stays
// open and unwritten: its fd never becomes ready, so a wake is
// spurious.
function idle_park(ms, k) {
  const ffi = require("bun:ffi");
  const sys = io_sys();
  const lib = ffi.dlopen(sys.mac ? "libSystem.dylib" : "libc.so.6", {
    pipe: { args: ["ptr"], returns: "i32" },
    setitimer: { args: ["i32", "ptr", "ptr"], returns: "i32" },
  }).symbols;
  process.on("SIGALRM", () => {});
  const it = new BigInt64Array([0n, 0n,
    BigInt(ms / 1000 | 0), BigInt(ms % 1000 * 1000)]);
  lib.setitimer(0, sys.ptr(it), null);
  const p = new Int32Array(2);
  lib.pipe(sys.ptr(p));
  sys.fcntl(p[0], 4, sys.fcntl(p[0], 3, 0) | (sys.mac ? 4 : 0x800));
  io_park_on(p[0], false, k, () => ({ $: CID(Unit) }));
  return undefined;
}

io_eff(CID(Idle.park), idle_park);
