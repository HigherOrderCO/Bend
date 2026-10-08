// bend-ids 1
// CID :zero BEND_CID_zero
// CID :full BEND_CID_full
// CID :frac BEND_CID_frac
// bend-ids end
function zero() {
  return 0;
}
function full() {
  return 4294967295;
}
function frac() {
  return 4.5;
}

io_eff(BEND_CID_zero, zero);
io_eff(BEND_CID_full, full);
io_eff(BEND_CID_frac, frac);
