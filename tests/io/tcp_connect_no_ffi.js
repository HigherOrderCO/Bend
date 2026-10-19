// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :ffi_absent BEND_CID_ffi_absent
// bend-ids end
// Make io_sys initialization fail once, as on a host without bun:ffi.
function ffi_absent_run() {
  const prior = Object.getOwnPropertyDescriptor(globalThis, "BEND_SYS");
  Object.defineProperty(globalThis, "BEND_SYS", {
    configurable: true,
    get() {
      if (prior) {
        Object.defineProperty(globalThis, "BEND_SYS", prior);
      } else {
        delete globalThis.BEND_SYS;
      }
      throw new Error("no ffi on this host");
    },
  });
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_ffi_absent, ffi_absent_run);
