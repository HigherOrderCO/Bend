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

io_eff(CID(low), low);
io_eff(CID(high), high);
io_eff(CID(apex), apex);
io_eff(CID(edge), edge);
io_eff(CID(torn), torn);
