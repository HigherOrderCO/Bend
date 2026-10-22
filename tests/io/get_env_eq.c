// Set a variable whose value holds a "=", for get_env to look past.
Term put_eq_run(Env e, Term* f, IoWork* w) {
  setenv("BEND_ENV_EQ", "x=y", 1);
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) put_eq_use(void) {
  io_eff(CID(put_eq), put_eq_run);
}
