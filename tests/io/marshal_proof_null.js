// bend-ids 1
// CID :keep BEND_CID_keep
// CID :give BEND_CID_give
// bend-ids end
function keep(e) {
  if (e === null) {
    return 7;
  }
  return 8;
}
function give() {
  return null;
}

io_eff(BEND_CID_keep, keep);
io_eff(BEND_CID_give, give);
