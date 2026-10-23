// File
// ====

// name is the path's bytes, or null if a value is past 255.
function file_open_with(name, mode) {
  if (name === null) {
    return io_fail(22);
  }
  if (name.includes(0)) {
    return io_fail(process.platform === "darwin" ? 92 : 84);
  }
  if (!["r", "w", "a", "x"].includes(mode)) {
    return io_fail(22);
  }
  try {
    // Node spells "x" as "wx".
    const fd = require("fs").openSync(name.length > 0 ? Buffer.from(name) : "",
      mode === "x" ? "wx" : mode, 0o644);
    return io_done(fd);
  } catch (e) {
    return io_fail(-e.errno);
  }
}

function file_open(path, mode) {
  return file_open_with(io_bytes(path), mode);
}

function file_open_bytes(path, mode) {
  return file_open_with(io_unlist(path), mode);
}

io_eff(CID(File.open), file_open);
io_eff(CID(File.open_bytes), file_open_bytes);
