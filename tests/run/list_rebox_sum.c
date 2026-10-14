static Term rebox_heap_run(Env e, Term* f, IoWork* w) {
  if (f[0] != f[1]) {
    err_fail("unexpected reboxing result");
  }
  u32 pages = a32_load(a32_at(e.mem, H_BUMP));
  if (f[2] && pages > f[2] + (QUANTUM >> PAGE_BITS)) {
    err_fail("list reboxing retains heap pages");
  }
  return (Term)pages;
}

static void __attribute__((constructor)) rebox_heap_use(void) {
  io_eff(CID(heap.check), rebox_heap_run);
}
