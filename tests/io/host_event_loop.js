io_eff(CID(Host.microtask_seen), (k) => {
  let seen = false;
  let polls = 0;
  Promise.resolve().then(() => { seen = true; });
  const more = () => {
    if (seen) return polls;
    if (polls === 20) return 4294967295;
    polls += 1;
    io_park_on(undefined, false, k, more, performance.now() + 10);
  };
  return more();
});

io_eff(CID(Host.settled), (k) => {
  io_park_promise(Promise.resolve(21), k, (value) => value * 2, (error) => error.errno);
});

io_eff(CID(Host.rejected), (k) => {
  io_park_promise(Promise.reject({ errno: 71 }), k, (value) => value * 2, (error) => error.errno);
});

io_eff(CID(Host.descriptor), (k) => {
  const sys = io_sys();
  const fds = new Int32Array(2);
  if (sys.pipe(sys.ptr(fds)) !== 0) throw new Error("test pipe creation failed");
  const byte = new Uint8Array(1);
  io_park_on(fds[0], false, k, () => {
    const count = Number(sys.read(fds[0], sys.ptr(byte), 1));
    fds.forEach((fd) => sys.close(fd));
    if (count !== 1) throw new Error("test pipe read failed");
    return byte[0];
  });
  setTimeout(() => require("node:fs").writeSync(fds[1], Uint8Array.of(43)), 10);
});
