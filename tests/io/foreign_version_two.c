#include <stddef.h>

#ifndef HID(COUNTER_HELPER)
#define HID(COUNTER_HELPER)
#define HID(INITIAL_VALUE) 70
#define HID(NEXT_CODE)(x) ((x).code[0] + 1)

typedef struct HID(CounterState) {
  u32 value;
  u32 code[1];
} HID(CounterState);
static HID(CounterState) HID(state) = { HID(INITIAL_VALUE), { 0 } };

static u32 HID(code)(void) {
  u32* used = (u32*)((char*)&HID(state) + offsetof(HID(CounterState), code));
  *used = HID(NEXT_CODE)(HID(state));
  return ++HID(state).value;
}
#undef HID(INITIAL_VALUE)
#undef HID(NEXT_CODE)
#endif

Term HID(now_run)(Env e, Term* f, IoWork* w) {
  u32 (*const next_value)(void) = HID(code);
  w->code = 0;
  return term_pak(CID(Instant), next_value());
}

static void __attribute__((constructor)) HID(now_use)(void) {
  io_eff(CID(now), HID(now_run));
}
