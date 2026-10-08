// bend-ids 1
// CID :twice BEND_CID_twice
// bend-ids end
function twice(x) {
  return (2 * x) >>> 0;
}

io_eff(BEND_CID_twice, twice);
