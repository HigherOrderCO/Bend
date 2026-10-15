// IO
// ==

function io_print(text) {
  io_out(1, io_bytes(text + "\n"));
  return { $: CID(Unit) };
}

io_eff(CID(IO.print), io_print);
