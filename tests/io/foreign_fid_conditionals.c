#ifdef FID(guard_only)
#error A presence guard kept an unused function alive
#endif
#if defined(\
  FID(guard_only))
#error A continued presence guard kept an unused function alive
#endif
#/**/ifdef FID(guard_only)
#error A commented directive kept an unused function alive
#endif
%:if defined(FID(guard_only))
%:error A digraph directive kept an unused function alive
%:endif
#ifndef FID(imul)
#error The macro call target lost its entry
#endif

#define CALL_FID /* A comment's newline does not end the directive.
                    The call target below still needs an entry. */ \
  FID(imul)

static Term answer_run(Env e, Term* f, IoWork* w) {
  (void)f;
  (void)w;
  u64 at = task_node(e, CALL_FID, TERM_HOLE, 0, 0);
  e.mem[at] = 6;
  e.mem[at + 1] = 7;
  Term result = corpus_eval(e.mem, term_tsk(CALL_FID, at));
  if (err_seen(e.mem)) err_fail("macro function entry failed");
  return result;
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(CID(answer), answer_run);
}
