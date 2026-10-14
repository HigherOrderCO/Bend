// Tag
// ===

// The macros are this module's spelling, from wherever the program sits.
Term tag_off_run(Env e, Term* f, IoWork* w) {
  return term_pak(CID(Off), 0);
}

Term tag_on_run(Env e, Term* f, IoWork* w) {
  return term_pak(CID(On), 3);
}

static void __attribute__((constructor)) tag_use(void) {
  io_eff(CID(tag.off), tag_off_run);
  io_eff(CID(tag.on), tag_on_run);
}
