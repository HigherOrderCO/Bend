// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :IO.spawn BEND_CID_IO_spawn
// bend-ids end
// IO
// ==

Term io_spawn_run(Env e, Term* f, IoWork* w) {
  io_spawn(f[0]);
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) io_spawn_use(void) {
  io_eff(BEND_CID_IO_spawn, io_spawn_run);
}
