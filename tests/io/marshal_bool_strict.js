// bend-ids 1
// CID :flop BEND_CID_flop
// CID :loose BEND_CID_loose
// bend-ids end
function flop(b) {
  return !b;
}
function loose(c) {
  return c ? "t" : 1;
}

io_eff(BEND_CID_flop, flop);
io_eff(BEND_CID_loose, loose);
