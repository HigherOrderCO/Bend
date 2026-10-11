// IO
// ==

Term io_print_run(Env e, Term* f, IoWork* w) {
  uint64_t n = 0;
  char* text = io_cstr(e, f[0], &n);
  io_out(stdout, text, n);
  io_out(stdout, "\n", 1);
  free(text);
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) io_print_use(void) {
  io_eff(CID(IO.print), io_print_run);
}
