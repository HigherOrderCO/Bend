// bend-ids 1
// CID :top BEND_CID_top
// CID :deep BEND_CID_deep
// bend-ids end
function top(p) {
  return typeof p.snd;
}
function deep(p) {
  return typeof p.snd.snd;
}

io_eff(BEND_CID_top, top);
io_eff(BEND_CID_deep, deep);
