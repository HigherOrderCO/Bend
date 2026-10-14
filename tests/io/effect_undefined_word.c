// The length of n copies of the word undefined, spelled in this comment
// and in a string: C the emitter never generated.
Term word_len_run(Env e, Term* f, IoWork* w) {
  return (Term)((u32)f[0] * (u32)strlen("undefined"));
}

static void __attribute__((constructor)) word_len_use(void) {
  io_eff(CID(word.len), word_len_run);
}
