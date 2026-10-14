function five() {
  return 5;
}
function zero() {
  return 0;
}
function wrap() {
  return {$: CID(Some), value: 9};
}
function dub(n) {
  return n * 2;
}
function flip(b) {
  return !b;
}
function ord() {
  return {$: CID(EQ)};
}

io_eff(CID(five), five);
io_eff(CID(zero), zero);
io_eff(CID(wrap), wrap);
io_eff(CID(dub), dub);
io_eff(CID(flip), flip);
io_eff(CID(ord), ord);
