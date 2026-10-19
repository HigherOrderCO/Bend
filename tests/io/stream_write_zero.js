function stream_write_zero() {
  const child = require("node:child_process");
  for (const fd of [1, 2]) {
    const code = `${io_out}
const fs = require("node:fs");
const write = fs.writeSync;
fs.writeSync = (fd, data, ...rest) =>
  fd === ${fd} && data.length > 0 ? 0 : write(fd, data, ...rest);
io_out(${fd}, Buffer.from("x"));
`;
    const got = child.spawnSync(process.execPath, ["-e", code], {
      encoding: "utf8", timeout: 2000,
    });
    if (got.status !== 1 || got.stdout !== "" || got.stderr !== (fd === 1
      ? "bend: a short write on a standard stream\n" : "")) {
      return fd;
    }
  }
  return 0;
}

io_eff(CID(Stream.write_zero), stream_write_zero);
