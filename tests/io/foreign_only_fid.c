static Term answer_run(Env e, Term* f, IoWork* w) {
  (void)f;
  (void)w;
  u64 at = task_node(e, FID(hidden), TERM_HOLE, 0, 0);
  e.mem[at] = 41;
  Term result = corpus_eval(e.mem, term_tsk(FID(hidden), at));
  if (err_seen(e.mem)) err_fail("foreign-only function failed");
  return result;
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(CID(answer), answer_run);
}
