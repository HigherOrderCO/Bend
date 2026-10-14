Term main_run(Env e, Term* f, IoWork* w) {
  io_out(stdout, "EFF RAN\n", 8);
  return (Term)7;
}

static void __attribute__((constructor)) main_use(void) {
  io_eff(CID(main), main_run);
}
