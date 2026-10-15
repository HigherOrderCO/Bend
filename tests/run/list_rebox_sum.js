io_eff(CID(heap.check), (value, expected) => {
  if (value !== expected) {
    throw new Error("unexpected reboxing result");
  }
  return 0;
});
