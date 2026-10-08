// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :AudioTest.count BEND_CID_AudioTest_count
// CID :AudioTest.start BEND_CID_AudioTest_start
// CID :AudioTest.wait BEND_CID_AudioTest_wait
// CID :AudioTest.closed BEND_CID_AudioTest_closed
// bend-ids end
#ifdef __linux__
#include <alsa/asoundlib.h>
#include <dlfcn.h>

// Writes of real samples (frame k is k, -k) get the case's result; the
// rest reach ALSA's null output. Cases 0-254: the first write takes
// 1-255 frames. Then: EINTR once, EPIPE once, every write capped at 17,
// 64 frames then EINTR, 64 then EPIPE (all deliver 512 frames in
// order), ENODEV always and 0 always (the pump stops after one write).
enum { SHORT = 255, EINTR1, EPIPE1, CAP17, PART_EINTR, PART_EPIPE, DEAD, ZERO, CASES };

static snd_pcm_t* audio_test_unit;
static void*      audio_test_lib;
static pthread_key_t audio_test_key;
static u32 audio_test_case;
static _Atomic(u32) audio_test_frames, audio_test_bad, audio_test_writes;
static _Atomic(u32) audio_test_recovers, audio_test_stopped;

static void audio_test_exit(void* p) {
  atomic_store(&audio_test_stopped, 1);
}

static void* audio_test_sym(const char* name) {
  void* p = snd_dlsym(audio_test_lib, name, NULL);
  if (p == NULL) err_fail("audio test: missing ALSA symbol");
  return p;
}

int snd_pcm_open(snd_pcm_t** p, const char* name, snd_pcm_stream_t s, int mode) {
  int (*open)(snd_pcm_t**, const char*, snd_pcm_stream_t, int) = audio_test_sym("snd_pcm_open");
  int code = open(p, "null", s, mode);
  if (code == 0) audio_test_unit = *p;
  return code;
}

typedef snd_pcm_sframes_t (*Write)(snd_pcm_t*, const void*, snd_pcm_uframes_t);

snd_pcm_sframes_t snd_pcm_writei(snd_pcm_t* p, const void* data, snd_pcm_uframes_t frames) {
  Write write = audio_test_sym("snd_pcm_writei");
  const float* pcm = data;
  pthread_setspecific(audio_test_key, (void*)1);
  bool real = false;
  for (u64 i = 0; i < frames; ++i) real |= pcm[i * 2] != 0;
  if (p == audio_test_unit && real) {
    u32 c    = audio_test_case;
    u32 call = atomic_fetch_add(&audio_test_writes, 1);
    if (call > 100000) err_fail("audio test: unbounded writes");
    if (c < SHORT && call == 0) frames = c + 1;
    if (c == CAP17 && frames > 17) frames = 17;
    if ((c == PART_EINTR || c == PART_EPIPE) && call == 0) frames = 64;
    if ((c == EINTR1 && call == 0) || (c == PART_EINTR && call == 1)) return -EINTR;
    if ((c == EPIPE1 && call == 0) || (c == PART_EPIPE && call == 1)) return -EPIPE;
    if (c == DEAD) return -ENODEV;
    if (c == ZERO) return 0;
  }
  snd_pcm_sframes_t n = write(p, data, frames);
  if (n > 0 && p == audio_test_unit) {
    u32 k = atomic_load(&audio_test_frames);
    for (snd_pcm_sframes_t i = 0; i < n; ++i) {
      if (pcm[i * 2] == 0) continue;
      ++k;
      if (pcm[i * 2] != (float)k || pcm[i * 2 + 1] != -(float)k) atomic_store(&audio_test_bad, 1);
    }
    atomic_store(&audio_test_frames, k);
  }
  return n;
}

int snd_pcm_recover(snd_pcm_t* p, int code, int silent) {
  int (*recover)(snd_pcm_t*, int, int) = audio_test_sym("snd_pcm_recover");
  if (p == audio_test_unit) atomic_fetch_add(&audio_test_recovers, 1);
  return recover(p, code, silent);
}
#endif

Term audio_test_count_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  return CASES;
#endif
  return 0;
}

Term audio_test_start_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  audio_test_case = (u32)f[0];
  atomic_store(&audio_test_frames, 0);
  atomic_store(&audio_test_bad, 0);
  atomic_store(&audio_test_writes, 0);
  atomic_store(&audio_test_recovers, 0);
  atomic_store(&audio_test_stopped, 0);
#endif
  return term_pak(BEND_CID_Unit, 0);
}

// Waits for all 512 frames, or for the pump to stop on a dead device.
Term audio_test_wait_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  u32  c    = audio_test_case;
  bool dead = c == DEAD || c == ZERO;
  u64  t0   = io_tick();
  while (dead ? !atomic_load(&audio_test_stopped) : atomic_load(&audio_test_frames) < 512) {
    if (atomic_load(&audio_test_bad) || io_tick() - t0 > 1000000000ull) {
      fprintf(stderr, "audio test case %u: frames=%u writes=%u recovers=%u\n", c,
        atomic_load(&audio_test_frames), atomic_load(&audio_test_writes),
        atomic_load(&audio_test_recovers));
      err_fail("audio test: pump lost or reordered frames");
    }
    struct timespec t = { 0, 100000 };
    nanosleep(&t, NULL);
  }
  if (dead && (atomic_load(&audio_test_writes) != 1
    || atomic_load(&audio_test_recovers) != (c == DEAD)))
    err_fail("audio test: dead device retried");
#endif
  return term_pak(BEND_CID_Unit, 0);
}

Term audio_test_closed_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  if (!atomic_load(&audio_test_stopped)) err_fail("audio test: close did not join the pump");
  if (audio_test_case != DEAD && audio_test_case != ZERO && atomic_load(&audio_test_frames) != 512)
    err_fail("audio test: final frames");
#endif
  return term_pak(BEND_CID_Unit, 0);
}

static void __attribute__((constructor)) audio_test_use(void) {
#ifdef __linux__
  audio_test_lib = snd_dlopen(NULL, RTLD_LAZY, NULL, 0);
  if (audio_test_lib == NULL || pthread_key_create(&audio_test_key, audio_test_exit))
    err_fail("audio test: setup");
#endif
  io_eff(BEND_CID_AudioTest_count, audio_test_count_run);
  io_eff(BEND_CID_AudioTest_start, audio_test_start_run);
  io_eff(BEND_CID_AudioTest_wait, audio_test_wait_run);
  io_eff(BEND_CID_AudioTest_closed, audio_test_closed_run);
}
