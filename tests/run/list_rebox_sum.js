// bend-ids 1
// CID :heap.check BEND_CID_heap_check
// bend-ids end
io_eff(BEND_CID_heap_check, (value, expected) => {
  if (value !== expected) {
    throw new Error("unexpected reboxing result");
  }
  return 0;
});
