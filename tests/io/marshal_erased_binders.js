// bend-ids 1
// CID :scale BEND_CID_scale
// CID :apply7 BEND_CID_apply7
// bend-ids end
function scale(x) {
  return (x + 100) >>> 0;
}
function apply7(f) {
  return f(7);
}

io_eff(BEND_CID_scale, scale);
io_eff(BEND_CID_apply7, apply7);
