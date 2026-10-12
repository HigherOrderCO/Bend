static u32 slab_seen;
static u32 slab_pages;

static Term slab_dispose_run(Env e, Term* f, IoWork* w) {
  Term closure = f[0];
  if (term_tag(closure) != TAG_CLO || term_loc(closure) < HEAP_OFF
    || fid_arity(term_aux(closure)) != 2) {
    err_fail("expected a closure owning an array");
  }
  Term capture = e.mem[term_loc(closure)];
  if ((term_tag(capture) != TAG_BUF && term_tag(capture) != TAG_ARR)
    || term_loc(capture) < HEAP_OFF || blk_cls(capture) != 4) {
    err_fail("expected a runtime array capture");
  }
  term_drop(e, closure);
  u32 pages = a32_load(a32_at(e.mem, H_BUMP));
  if (++slab_seen == 1024) {
    slab_pages = pages;
  }
  if (slab_seen > 1024 && pages > slab_pages + (QUANTUM >> PAGE_BITS)) {
    err_fail("closure disposal retains heap pages");
  }
  return (Term)5;
}

static void __attribute__((constructor)) slab_dispose_use(void) {
  io_eff(CID(dispose), slab_dispose_run);
}
