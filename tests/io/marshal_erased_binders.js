function scale(x) {
  return (x + 100) >>> 0;
}
function apply7(f) {
  return f(7);
}

io_eff(CID(scale), scale);
io_eff(CID(apply7), apply7);
