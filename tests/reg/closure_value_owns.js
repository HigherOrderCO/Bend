// bend-ids 1
// CID :heap.check BEND_CID_heap_check
// bend-ids end
io_eff(BEND_CID_heap_check, (value, expected) => {
  if (value !== expected) {
    throw new Error("closure_value_owns: incorrect sum");
  }
  return 0;
});
