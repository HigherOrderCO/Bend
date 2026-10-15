function keep(e) {
  if (e === null) {
    return 7;
  }
  return 8;
}
function give() {
  return null;
}

io_eff(CID(keep), keep);
io_eff(CID(give), give);
