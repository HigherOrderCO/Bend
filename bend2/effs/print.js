// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.print BEND_CID_IO_print
// bend-ids end
// IO
// ==

function io_print(text) {
  io_out(1, io_bytes(text + "\n"));
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_IO_print, io_print);
