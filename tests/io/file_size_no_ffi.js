// Takes the FFI table away until ffi_back puts it back, so File.size runs
// as on a host without bun:ffi and nothing after it in the process does.
let ffi_real = undefined;

function ffi_absent_run() {
  ffi_real = io_sys();
  globalThis.BEND_SYS = new Proxy(ffi_real, {
    get() {
      throw new Error("no ffi table on this host");
    },
  });
  return { $: CID(Unit) };
}

function ffi_back_run() {
  globalThis.BEND_SYS = ffi_real;
  return { $: CID(Unit) };
}

io_eff(CID(ffi_absent), ffi_absent_run);
io_eff(CID(ffi_back), ffi_back_run);
