// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :hold BEND_CID_hold
// bend-ids end
#include <time.h>

// Blocks the loop for ms milliseconds inside one effect.
Term hold_run(Env e, Term* f, IoWork* w) {
  struct timespec t = { (u32)f[0] / 1000, (long)((u32)f[0] % 1000) * 1000000 };
  while (nanosleep(&t, &t) != 0) {}
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) hold_use(void) {
  io_eff(BEND_CID_hold, hold_run);
}
