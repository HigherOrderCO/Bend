static Term wide_capture_inspect_run(Env e, Term* f, IoWork* w) {
  if (term_tag(f[0]) != TAG_CLO || fid_arity(term_aux(f[0])) != f[1]) {
    err_fail("incorrect wide capture layout");
  }
  u64 at = task_node(e, FID(Clo~apply), TERM_HOLE, 0, 0);
  e.mem[at] = f[0];
  e.mem[at + 1] = 7;
  Term result = corpus_eval(e.mem, term_tsk(FID(Clo~apply), at));
  if (err_seen(e.mem)) err_fail("wide capture application failed");
  return result;
}

static void __attribute__((constructor)) wide_capture_inspect_use(void) {
  io_eff(CID(inspect), wide_capture_inspect_run);
}
