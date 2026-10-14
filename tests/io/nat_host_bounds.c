Term get_run(Env e, Term* f, IoWork* w) {
  u32 k = (u32)f[0];
  return k == 0 ? (Term)281474976710655ull
    : k == 1 ? (Term)281474976710656ull : (Term)9007199254740992ull;
}

static void __attribute__((constructor)) get_use(void) {
  io_eff(CID(get), get_run);
}
