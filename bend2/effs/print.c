// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.print BEND_CID_IO_print
// bend-ids end
// IO
// ==

Term io_print_run(Env e, Term* f, IoWork* w) {
  uint64_t n = 0;
  char* text = io_cstr(e, f[0], &n);
  text[n] = '\n';
  io_out(stdout, text, n + 1);
  free(text);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) io_print_use(void) {
  io_eff(BEND_CID_IO_print, io_print_run);
}
