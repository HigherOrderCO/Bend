// IO
// ==

Term twice_run(Env e, Term* f, IoWork* w) {
  return (Term)(uint32_t)(2 * (uint32_t)f[0]);
}

static void __attribute__((constructor)) twice_use(void) {
  io_eff(CID(twice), twice_run);
}
