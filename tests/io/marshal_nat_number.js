// bend-ids 1
// CID :zero BEND_CID_zero
// CID :five BEND_CID_five
// bend-ids end
function zero() {
  return 0;
}
function five() {
  return 5;
}

io_eff(BEND_CID_zero, zero);
io_eff(BEND_CID_five, five);
