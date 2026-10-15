// A child writes len bytes into a FIFO, then waits on an ack pipe and
// closes the FIFO only once the reader has drained it and parks again, so
// the reader's last wake is the close alone. The reader's end moves to fd
// hi when hi is nonzero, past FD_SETSIZE if hi is.
function fifo_drain(len, hi, k) {
  const ffi = require("bun:ffi");
  const sys = io_sys();
  const lib = ffi.dlopen(sys.mac ? "libSystem.dylib" : "libc.so.6", {
    mkfifo: { args: ["cstring", "u32"], returns: "i32" },
    unlink: { args: ["cstring"], returns: "i32" },
    open: { args: ["cstring", "i32"], returns: "i32" },
    dup2: { args: ["i32", "i32"], returns: "i32" },
    pipe: { args: ["ptr"], returns: "i32" },
  }).symbols;
  const path = Buffer.from(`/tmp/bend_fifo_js_${process.pid}\0`);
  const nonblock = sys.mac ? 0x4 : 0x800;
  lib.unlink(path);
  lib.mkfifo(path, 0o600);
  let rd = lib.open(path, nonblock);
  const wr = lib.open(path, 1 | nonblock);
  lib.unlink(path);
  if (hi !== 0) {
    lib.dup2(rd, hi);
    sys.close(rd);
    rd = hi;
  }
  const ack = new Int32Array(2);
  lib.pipe(sys.ptr(ack));
  sys.fcntl(ack[1], 2, 1);
  const sh = `printf %s "${"x".repeat(len)}"; head -c 1 >/dev/null`;
  Bun.spawn(["sh", "-c", sh], { stdin: ack[0], stdout: wr });
  sys.close(ack[0]);
  sys.close(wr);
  const b = new Uint8Array(64);
  const again = sys.mac ? 35 : 11;
  let total = 0;
  let acked = false;
  const go = () => {
    for (;;) {
      const n = Number(sys.read(rd, sys.ptr(b), 64));
      if (n > 0) {
        total += n;
        continue;
      }
      if (n < 0 && sys.errno() === again) {
        if (!acked && total === len) {
          sys.close(ack[1]);
          acked = true;
        }
        io_park_on(rd, false, k, go);
        return undefined;
      }
      sys.close(rd);
      return total;
    }
  };
  return go();
}

io_eff(CID(fifo.drain), fifo_drain);
