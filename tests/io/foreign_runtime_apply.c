static Term native_apply_run(Env e, Term* f, IoWork* w) {
  (void)w;
  u64 at = task_node(e, FID(Clo~apply), TERM_HOLE, 0, 0);
  e.mem[at] = f[0];
  e.mem[at + 1] = f[1];
  Term result = corpus_eval(e.mem, term_tsk(FID(Clo~apply), at));
  if (err_seen(e.mem)) err_fail("foreign closure application failed");
  return result;
}

static void __attribute__((constructor)) native_apply_use(void) {
  io_eff(CID(native.apply), native_apply_run);
}
