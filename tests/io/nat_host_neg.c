Term get_run(Env e, Term* f, IoWork* w) {
  return (Term)(u64)-3;
}

static void __attribute__((constructor)) get_use(void) {
  io_eff(CID(get), get_run);
}
