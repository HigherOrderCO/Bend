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
  return { $: CID(Unit) };
}

io_eff(CID(ffi_absent), ffi_absent_run);
