// Test
// ====

// Hold.on starts a thread that takes io_owner, as an IO.eval does, and
// returns once that thread holds it; until Hold.off, every evaluation on
// the IO loop finds the pool owned and must run without it. Hold.off
// releases the lock and returns once it is released.

static pthread_mutex_t hold_gate = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  hold_bell = PTHREAD_COND_INITIALIZER;
static u32             hold_state;
static pthread_t       hold_tid;

static void hold_set(u32 s) {
  pthread_mutex_lock(&hold_gate);
  hold_state = s;
  pthread_cond_broadcast(&hold_bell);
  pthread_mutex_unlock(&hold_gate);
}

static void hold_wait(u32 s) {
  pthread_mutex_lock(&hold_gate);
  while (hold_state != s) {
    pthread_cond_wait(&hold_bell, &hold_gate);
  }
  pthread_mutex_unlock(&hold_gate);
}

static void* hold_main(void* arg) {
  pthread_mutex_lock(&io_owner);
  hold_set(1);
  hold_wait(2);
  pthread_mutex_unlock(&io_owner);
  return NULL;
}

Term hold_on_run(Env e, Term* f, IoWork* w) {
  if (pthread_create(&hold_tid, NULL, hold_main, NULL)) {
    err_fail("pthread_create");
  }
  hold_wait(1);
  return term_pak(CID(Unit), 0);
}

Term hold_off_run(Env e, Term* f, IoWork* w) {
  hold_set(2);
  pthread_join(hold_tid, NULL);
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) hold_use(void) {
  io_eff(CID(Hold.on), hold_on_run);
  io_eff(CID(Hold.off), hold_off_run);
}
