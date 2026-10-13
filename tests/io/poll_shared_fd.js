function shared_fd_read(k) {
  const ffi = require("bun:ffi");
  const sys = io_sys();
  const lib = ffi.dlopen(sys.mac ? "libSystem.dylib" : "libc.so.6", {
    socketpair: { args: ["i32", "i32", "i32", "ptr"], returns: "i32" },
  }).symbols;
  const pair = new Int32Array(2);
  const input = new Uint8Array([97, 98, 99]);
  const byte = new Uint8Array(1);
  if (lib.socketpair(1, 1, 0, sys.ptr(pair))
    || Number(sys.send(pair[1], sys.ptr(input), 3, 0)) !== 3)
    throw new Error("shared fd socket setup failed");
  const fd = pair[0];
  let seen = 0;
  const root_more = () => {
    if (Number(sys.read(fd, sys.ptr(byte), 1)) !== 1
      || byte[0] !== (seen === 0 ? 97 : 99))
      throw new Error("shared fd resumed out of order");
    seen++;
    if (seen === 1) {
      io_park_on(fd, false, k, root_more);
      return undefined;
    }
    if (seen !== 3) throw new Error("shared fd lost its second waiter");
    sys.close(pair[0]);
    sys.close(pair[1]);
    return seen;
  };
  io_park_on(fd, false, k, root_more);
  io_park_on(fd, false, (x) => ({ $: "Emit", value: x }), () => {
    if (seen !== 1 || Number(sys.read(fd, sys.ptr(byte), 1)) !== 1 || byte[0] !== 98)
      throw new Error("shared fd displaced its second waiter");
    seen++;
    return { $: CID(Unit) };
  });
  globalThis.BEND_IO.live++;
  return undefined;
}

io_eff(CID(Shared.read), shared_fd_read);
