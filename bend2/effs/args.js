// IO
// ==

function io_args() {
  let xs = { $: CID(Nil) };
  for (let i = cli_args.length; i > 0; i -= 1) {
    xs = { $: CID(Con), head: cli_args[i - 1], tail: xs };
  }
  return xs;
}

// Node and Bun decode the arguments before a program sees them, so a byte
// that isn't UTF-8 arrives as U+FFFD. Each argument takes the last raw one,
// before the one the next argument took, that decodes to it; with no such
// one (no raw ones, or the head, which the host makes absolute), its UTF-8.
function io_args_bytes() {
  const raw = cli_args.some((a) => a.includes("�")) ? io_args_raw() : [];
  let xs = { $: CID(Nil) };
  for (let i = cli_args.length, at = raw.length; i > 0; i -= 1) {
    let k = at - 1;
    while (k >= 0 && io_text(raw[k], raw[k].length) !== cli_args[i - 1]) {
      k -= 1;
    }
    at = k < 0 ? at : k;
    const b = k < 0 ? io_bytes(cli_args[i - 1]) : raw[k];
    xs = { $: CID(Con), head: io_list(b, b.length), tail: xs };
  }
  return xs;
}

// The process's arguments as the OS holds them: Linux's /proc/self/cmdline,
// or macOS's _NSGetArgv through bun:ffi; none under node on macOS.
function io_args_raw() {
  try {
    if (process.platform === "linux") {
      const b = require("fs").readFileSync("/proc/self/cmdline");
      const xs = [];
      for (let at = 0, k; (k = b.indexOf(0, at)) >= 0; at = k + 1) {
        xs.push(b.subarray(at, k));
      }
      return xs;
    }
    const ffi = require("bun:ffi");
    const get = { args: [], returns: "ptr" };
    const sys = ffi.dlopen("libSystem.dylib",
      { _NSGetArgc: get, _NSGetArgv: get }).symbols;
    const argv = ffi.read.ptr(sys._NSGetArgv(), 0);
    return Array.from({ length: ffi.read.i32(sys._NSGetArgc(), 0) }, (_, i) => {
      const p = ffi.read.ptr(argv, 8 * i);
      let n = 0;
      while (ffi.read.u8(p, n) !== 0) {
        n += 1;
      }
      return Uint8Array.from({ length: n }, (_, j) => ffi.read.u8(p, j));
    });
  } catch (_) {
    return [];
  }
}

io_eff(CID(IO.args), io_args);
io_eff(CID(IO.args_bytes), io_args_bytes);
