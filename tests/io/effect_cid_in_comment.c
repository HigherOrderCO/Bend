// A wrong id fails the build with "CID(...) names no constructor or def",
// and so did this comment, which only quotes it. /* FID(nothing) too. */
Term answer_run(Env e, Term* f, IoWork* w) {
  return (Term)((u32)f[0] + 2);
}

static void __attribute__((constructor)) answer_use(void) {
  io_eff(CID(answer), answer_run);
}
