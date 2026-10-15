function zero() {
  return 0;
}
function full() {
  return 4294967295;
}
function frac() {
  return 4.5;
}

io_eff(CID(zero), zero);
io_eff(CID(full), full);
io_eff(CID(frac), frac);
