// bend-ids 1
// CID :gauge BEND_CID_gauge
// CID :pick BEND_CID_pick
// bend-ids end
function gauge(t) {
  if (t === null) {
    return 7;
  }
  return 13;
}
function pick() {
  return null;
}

io_eff(BEND_CID_gauge, gauge);
io_eff(BEND_CID_pick, pick);
