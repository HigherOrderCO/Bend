// IO
// ==

function io_print_err(text) {
  io_errs(text);
  return { $: CID(Unit) };
}

io_eff(CID(IO.print_err), io_print_err);
