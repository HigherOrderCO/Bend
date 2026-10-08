// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.write BEND_CID_IO_write
// bend-ids end
// IO
// ==

function io_write(text) {
  io_out(1, io_bytes(text));
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_IO_write, io_write);
