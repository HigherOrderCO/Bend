function low() {
  return str_in("\uD7FF");
}
function high() {
  return str_in("\uE000");
}
function apex() {
  return str_in("\u{10FFFF}");
}
function edge() {
  return str_in("\uD7FF");
}
function torn() {
  return str_in("a\uD800");
}

io_eff(CID(low), low);
io_eff(CID(high), high);
io_eff(CID(apex), apex);
io_eff(CID(edge), edge);
io_eff(CID(torn), torn);
