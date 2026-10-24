// File
// ====

// read(2) answers what is there, not what was asked: a pipe, a terminal
// and a short read all stop before max. Fill the buffer or reach EOF, so
// a caller that asked for n bytes gets n unless the file ends first.
function file_read_with(file, max, offset, pack) {
  const sys = io_sys();
  const len = Math.min(max, 2147483647);
  const b = new Uint8Array(Math.max(len, 1));
  let got = 0;
  while (got < len) {
    const part = b.subarray(got);
    const n = Number(offset === null ? sys.read(file, sys.ptr(part), len - got)
      : sys.pread(file, sys.ptr(part), len - got, BigInt(offset) + BigInt(got)));
    if (n < 0) {
      return io_tup(file, io_fail(sys.errno()));
    }
    if (n === 0) {
      break;
    }
    got += n;
  }
  return io_tup(file, io_done(pack(b, got)));
}

function file_read(file, max) {
  return file_read_with(file, max, null, io_text);
}

function file_read_bytes(file, max) {
  return file_read_with(file, max, null, io_list);
}

function file_read_at(file, offset, max) {
  return file_read_with(file, max, offset, io_list);
}

io_eff(CID(File.read), file_read);
io_eff(CID(File.read_bytes), file_read_bytes);
io_eff(CID(File.read_at), file_read_at);
