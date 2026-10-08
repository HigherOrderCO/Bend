// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.print_err BEND_CID_IO_print_err
// bend-ids end
// IO
// ==

function io_print_err(text) {
  io_errs(text);
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_IO_print_err, io_print_err);
