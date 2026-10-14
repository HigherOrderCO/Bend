io_eff(CID(heap.check), (value, expected) => {
  if (value !== expected) {
    throw new Error("closure_value_owns: incorrect sum");
  }
  return 0;
});
