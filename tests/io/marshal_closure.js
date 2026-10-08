// bend-ids 1
// CID #Some BEND_CID_Some
// CID :fold2 BEND_CID_fold2
// CID :pick_fn BEND_CID_pick_fn
// CID :stepper BEND_CID_stepper
// bend-ids end
function fold2(f) {
  return f(f(1)(2))(3);
}
function pick_fn(m) {
  if (m.$ === BEND_CID_Some) {
    return m.value(40);
  }
  return 0;
}
function stepper() {
  return {$: BEND_CID_Some, value: (x) => (x + 100) >>> 0};
}

io_eff(BEND_CID_fold2, fold2);
io_eff(BEND_CID_pick_fn, pick_fn);
io_eff(BEND_CID_stepper, stepper);
