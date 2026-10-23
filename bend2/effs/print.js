// IO
// ==

function io_print(text) {
  io_std(io_bytes(text + "\n"));
  return { $: CID(Unit) };
}

io_eff(CID(IO.print), io_print);
