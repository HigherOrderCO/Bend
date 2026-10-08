// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :File.write BEND_CID_File_write
// CID :File.write_bytes BEND_CID_File_write_bytes
// bend-ids end
// File
// ====

function file_write_buffer(file, b) {
  const fs = require("fs");
  let at = 0;
  try {
    while (at < b.length) {
      const n = fs.writeSync(file, b, at, b.length - at, null);
      // no progress would retry forever (#1445)
      if (n === 0) {
        return io_tup(file, io_fail(5));
      }
      at += n;
    }
    return io_tup(file, io_done({ $: BEND_CID_Unit }));
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

io_eff(BEND_CID_File_write, file_write);
io_eff(BEND_CID_File_write_bytes, file_write_bytes);
