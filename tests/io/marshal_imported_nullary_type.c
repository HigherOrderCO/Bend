// bend-ids 1
// CID #Off BEND_CID_Off
// CID #On BEND_CID_On
// CID :tag.off BEND_CID_tag_off
// CID :tag.on BEND_CID_tag_on
// bend-ids end
// Tag
// ===

// The macros are this module's spelling, from wherever the program sits.
Term tag_off_run(Env e, Term* f, IoWork* w) {
  return term_pak(BEND_CID_Off, 0);
}

Term tag_on_run(Env e, Term* f, IoWork* w) {
  return term_pak(BEND_CID_On, 3);
}

static void __attribute__((constructor)) tag_use(void) {
  io_eff(BEND_CID_tag_off, tag_off_run);
  io_eff(BEND_CID_tag_on, tag_on_run);
}
