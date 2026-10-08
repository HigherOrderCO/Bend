// bend-ids 1
// CID :star BEND_CID_star
// CID :wide BEND_CID_wide
// bend-ids end
function star() {
  return "g";
}
function wide(c) {
  return c + "b";
}

io_eff(BEND_CID_star, star);
io_eff(BEND_CID_wide, wide);
