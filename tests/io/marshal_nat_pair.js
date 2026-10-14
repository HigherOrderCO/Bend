function top(p) {
  return typeof p.snd;
}
function deep(p) {
  return typeof p.snd.snd;
}

io_eff(CID(top), top);
io_eff(CID(deep), deep);
