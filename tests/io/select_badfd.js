function readiness_closed() {
  const sys = io_sys();
  const fd = sys.socket(2, 2, 0);
  if (fd < 0) return io_fail(sys.errno());
  if (sys.close(fd) < 0) return io_fail(sys.errno());
  return io_done(fd);
}

function readiness_idle(k) {
  const sys = io_sys();
  const ffi = require("bun:ffi");
  const lib = ffi.dlopen(sys.mac ? "libSystem.dylib" : "libc.so.6", {
    pipe: { args: ["ptr"], returns: "i32" },
  }).symbols;
  const p = new Int32Array(2);
  if (lib.pipe(sys.ptr(p)) < 0) throw "the test pipe failed";
  io_park_on(p[0], false, k, () => ({ $: CID(Unit) }));
  return undefined;
}

io_eff(CID(Readiness.closed), readiness_closed);
io_eff(CID(Readiness.idle), readiness_idle);
