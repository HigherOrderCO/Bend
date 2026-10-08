// bend-ids 1
// CID :wrap BEND_CID_wrap
// bend-ids end
function wrap(g) {
  return (x) => (g(x) + 100) >>> 0;
}

io_eff(BEND_CID_wrap, wrap);
