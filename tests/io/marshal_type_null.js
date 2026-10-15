function gauge(t) {
  if (t === null) {
    return 7;
  }
  return 13;
}
function pick() {
  return null;
}

io_eff(CID(gauge), gauge);
io_eff(CID(pick), pick);
