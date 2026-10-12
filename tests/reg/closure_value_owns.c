static Term closure_heap_run(Env e, Term* f, IoWork* w) {
  if (f[0] != f[1]) {
    err_fail("closure_value_owns: incorrect sum");
  }
  u32 pages = a32_load_acq(a32_at(e.mem, H_BUMP));
  if (f[2] && pages > f[2] + (QUANTUM >> PAGE_BITS)) {
    err_fail("closure_value_owns: arena grew after warm-up");
  }
  return pages;
}

static void __attribute__((constructor)) closure_heap_use(void) {
  io_eff(CID(heap.check), closure_heap_run);
}
