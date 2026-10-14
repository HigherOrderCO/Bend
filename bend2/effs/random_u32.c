// IO
// ==

#ifdef __linux__
#include <sys/random.h>
#endif

// One word from the host's entropy source: getrandom never returns short
// for a request this small once the pool is ready.
static void io_random_u32_call(IoWork* w) {
#ifdef __APPLE__
  arc4random_buf(&w->word, sizeof(w->word));
  w->code = 0;
#else
  io_sys_end(w, getrandom(&w->word, sizeof(w->word), 0));
#endif
}

static Term io_random_u32_pack(Env e, IoWork* w) {
  return io_res(e, w, w->word);
}

Term io_random_u32_run(Env e, Term* f, IoWork* w) {
  return io_work(w, io_random_u32_call, io_random_u32_pack);
}

static void __attribute__((constructor)) io_random_u32_use(void) {
  io_eff(CID(IO.random_u32), io_random_u32_run);
}
