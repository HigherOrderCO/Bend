// IO
// ==

Term io_spawn_run(Env e, Term* f, IoWork* w) {
  io_spawn(f[0]);
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) io_spawn_use(void) {
  io_eff(CID(IO.spawn), io_spawn_run);
}
