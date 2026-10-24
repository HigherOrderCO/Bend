function short_file(n) {
  const fs = require("fs");
  const cp = require("child_process");
  const path = "/tmp/bend_read_short_" + process.pid;
  try { fs.unlinkSync(path); } catch (e) {}
  cp.execFileSync("mkfifo", [path]);
  const rd = fs.openSync(path, fs.constants.O_RDONLY | fs.constants.O_NONBLOCK);
  const wr = fs.openSync(path, fs.constants.O_WRONLY | fs.constants.O_NONBLOCK);
  fs.unlinkSync(path);
  cp.spawn("/bin/sh", ["-c",
    "i=0; while [ $i -lt " + n + " ]; do printf x >&3; i=$((i+1)); sleep 0.001; done"],
    { stdio: ["ignore", "ignore", "ignore", wr] });
  fs.closeSync(wr);
  io_sys().fcntl(rd, 4, 0);
  return rd;
}

io_eff(CID(short.file), short_file);
