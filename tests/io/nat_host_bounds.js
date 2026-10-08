// bend-ids 1
// CID :get BEND_CID_get
// bend-ids end
function get(k) {
  return [281474976710655, 281474976710656, 9007199254740992][k];
}

io_eff(BEND_CID_get, get);
