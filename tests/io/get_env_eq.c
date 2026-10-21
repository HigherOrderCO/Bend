// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :put_eq BEND_CID_put_eq
// bend-ids end
// Set a variable whose value holds a "=", for get_env to look past.
Term put_eq_run(Env e, Term* f, IoWork* w) {
  setenv("BEND_ENV_EQ", "x=y", 1);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) put_eq_use(void) {
  io_eff(BEND_CID_put_eq, put_eq_run);
}
