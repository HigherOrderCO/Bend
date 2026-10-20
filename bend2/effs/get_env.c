// IO
// ==

Term io_get_env_run(Env e, Term* f, IoWork* w) {
  uint64_t n = 0;
  char* name = io_cstr(e, f[0], &n);
  // getenv("A=B") may match the entry "A=B=C" and answer part of A's value.
  const char* got = io_nul(name, n) || strchr(name, '=') != NULL ? NULL
    : getenv(name);
  free(name);
  return got == NULL ? io_fail(e, ENOENT, NULL)
    : io_done(e, io_str(e, got, strlen(got)));
}

static void __attribute__((constructor)) io_get_env_use(void) {
  io_eff(CID(IO.get_env), io_get_env_run);
}
