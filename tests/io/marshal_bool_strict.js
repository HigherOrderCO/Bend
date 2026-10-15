function flop(b) {
  return !b;
}
function loose(c) {
  return c ? "t" : 1;
}

io_eff(CID(flop), flop);
io_eff(CID(loose), loose);
