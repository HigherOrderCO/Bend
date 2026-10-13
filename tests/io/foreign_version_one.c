#include <stddef.h>

#ifndef COUNTER_HELPER
#define COUNTER_HELPER
#define INITIAL_VALUE 7
#define NEXT_CODE(x) ((x).code[0] + 1)

static u32 code(void) {
  typedef struct CounterState {
    u32 value;
    u32 code[1];
  } CounterState;
  static CounterState state = { INITIAL_VALUE, { 0 } };
  u32* used = (u32*)((char*)&state + offsetof(CounterState, code));
  *used = NEXT_CODE(state);
  return ++state.value;
}
#undef INITIAL_VALUE
#undef NEXT_CODE
#endif

Term now_run(Env e, Term* f, IoWork* w) {
  u32 (*const next_value)(void) = code;
  w->code = 0;
  return term_pak(CID(Instant), next_value());
}

static void __attribute__((constructor)) now_use(void) {
  io_eff(CID(now), now_run);
}
