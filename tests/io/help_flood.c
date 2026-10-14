// Flood
// =====

static void flood_tick_call(IoWork* w) {
  w->size = w->word;
}

static Term flood_tick_pack(Env e, IoWork* w) {
  return (Term)w->size;
}

Term flood_tick_run(Env e, Term* f, IoWork* w) {
  w->word = (uint32_t)f[0];
  return io_work(w, flood_tick_call, flood_tick_pack);
}

static void __attribute__((constructor)) flood_tick_use(void) {
  io_eff(CID(Flood.tick), flood_tick_run);
}
