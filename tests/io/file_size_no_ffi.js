// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :ffi_absent BEND_CID_ffi_absent
// CID :ffi_back BEND_CID_ffi_back
// bend-ids end
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
  return { $: BEND_CID_Unit };
}

function ffi_back_run() {
  globalThis.BEND_SYS = ffi_real;
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_ffi_absent, ffi_absent_run);
io_eff(BEND_CID_ffi_back, ffi_back_run);
