// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.write BEND_CID_IO_write
// bend-ids end
// IO
// ==

Term io_write_run(Env e, Term* f, IoWork* w) {
  uint64_t n = 0;
  char* text = io_cstr(e, f[0], &n);
  io_out(stdout, text, n);
  free(text);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) io_write_use(void) {
  io_eff(BEND_CID_IO_write, io_write_run);
}
