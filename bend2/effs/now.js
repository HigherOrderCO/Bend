// IO
// ==

function io_now() {
  return Math.floor(performance.now());
}

io_eff(CID(IO.now), io_now);
