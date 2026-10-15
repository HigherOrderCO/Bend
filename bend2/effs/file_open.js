// File
// ====

function file_open(path, mode) {
  const name = io_bytes(path);
  if (name.includes(0)) {
    return io_fail(process.platform === "darwin" ? 92 : 84);
  }
  if (!["r", "w", "a"].includes(mode)) {
    return io_fail(22);
  }
  try {
    const fd = require("fs")
      .openSync(name.length > 0 ? Buffer.from(name) : "", mode, 0o644);
    return io_done(fd);
  } catch (e) {
    return io_fail(-e.errno);
  }
}

io_eff(CID(File.open), file_open);
