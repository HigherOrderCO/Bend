// bend-ids 1
// CID :uint_zero BEND_CID_uint_zero
// CID :float_zero BEND_CID_float_zero
// bend-ids end
Term uint_zero_run(Env e, Term* f, IoWork* w) {
  return 0;
}

Term float_zero_run(Env e, Term* f, IoWork* w) {
  return f32_rewrap(-0.0f);
}

static void __attribute__((constructor)) zero_use(void) {
  io_eff(BEND_CID_uint_zero, uint_zero_run);
  io_eff(BEND_CID_float_zero, float_zero_run);
}
