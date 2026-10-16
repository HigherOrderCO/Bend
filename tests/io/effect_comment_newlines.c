Term probe_run(Env e, Term* f, IoWork* w) {
  return io_box(e, C\
ID(Some), 42);
}
static void __attribute__((constructor)) probe_use(void) { io_eff(CID(probe), probe_run); }
