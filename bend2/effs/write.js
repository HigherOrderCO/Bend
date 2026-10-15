// IO
// ==

function io_write(text) {
  io_out(1, io_bytes(text));
  return { $: CID(Unit) };
}

io_eff(CID(IO.write), io_write);
