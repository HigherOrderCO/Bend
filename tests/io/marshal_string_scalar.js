// bend-ids 1
// CID :low BEND_CID_low
// CID :high BEND_CID_high
// CID :apex BEND_CID_apex
// CID :edge BEND_CID_edge
// CID :torn BEND_CID_torn
// bend-ids end
function low() {
  return "\uD7FF";
}
function high() {
  return "\uE000";
}
function apex() {
  return "\u{10FFFF}";
}
function edge() {
  return "\uD7FF";
}
function torn() {
  return "a\uD800";
}

io_eff(BEND_CID_low, low);
io_eff(BEND_CID_high, high);
io_eff(BEND_CID_apex, apex);
io_eff(BEND_CID_edge, edge);
io_eff(BEND_CID_torn, torn);
