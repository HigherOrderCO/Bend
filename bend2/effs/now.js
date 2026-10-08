// bend-ids 1
// CID :IO.now BEND_CID_IO_now
// bend-ids end
// IO
// ==

function io_now() {
  return Math.floor(performance.now());
}

io_eff(BEND_CID_IO_now, io_now);
