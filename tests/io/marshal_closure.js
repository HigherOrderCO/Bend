function fold2(f) {
  return f(f(1)(2))(3);
}
function pick_fn(m) {
  if (m.$ === CID(Some)) {
    return m.value(40);
  }
  return 0;
}
function stepper() {
  return {$: CID(Some), value: (x) => (x + 100) >>> 0};
}

io_eff(CID(fold2), fold2);
io_eff(CID(pick_fn), pick_fn);
io_eff(CID(stepper), stepper);
