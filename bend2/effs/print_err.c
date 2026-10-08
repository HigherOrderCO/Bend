// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.print_err BEND_CID_IO_print_err
// bend-ids end
// IO
// ==

Term io_print_err_run(Env e, Term* f, IoWork* w) {
  io_errs(e, f[0]);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) io_print_err_use(void) {
  io_eff(BEND_CID_IO_print_err, io_print_err_run);
}
