// File
// ====

function file_write_buffer(file, b) {
  const fs = require("fs");
  let at = 0;
  try {
    while (at < b.length) {
      at += fs.writeSync(file, b, at, b.length - at, null);
    }
    return io_tup(file, io_done({ $: CID(Unit) }));
  } catch (e) {
    return io_tup(file, io_fail(Math.abs(e.errno ?? 5)));
  }
}

function file_write(file, data) {
  return file_write_buffer(file, io_bytes(data));
}

function file_write_bytes(file, data) {
  const b = io_unlist(data);
  return b === null ? io_tup(file, io_fail(22)) : file_write_buffer(file, b);
}

io_eff(CID(File.write), file_write);
io_eff(CID(File.write_bytes), file_write_bytes);
