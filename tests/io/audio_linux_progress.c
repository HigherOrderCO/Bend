#ifdef __linux__
#include <alsa/asoundlib.h>
#include <dlfcn.h>
#include <spawn.h>
#include <sys/wait.h>

// Only this program's null output is affected. Positive writes reach ALSA.
// A thread key reports pump exit without reading its private state.
static snd_pcm_t* audio_test_unit;
static void* audio_test_library;
static pthread_key_t audio_test_key;
static u32 audio_test_mode, audio_test_width;
static _Atomic(u32) audio_test_frames, audio_test_bad, audio_test_fault;
static _Atomic(u32) audio_test_writes, audio_test_recovers, audio_test_resumes;
static _Atomic(u32) audio_test_finished, audio_test_closes;

static void audio_test_exit(void* p) {
  atomic_store(&audio_test_finished, 1);
}

static void* audio_test_symbol(const char* name) {
  void* p = snd_dlsym(audio_test_library, name, NULL);
  if (p == NULL) err_fail("audio test: missing ALSA symbol");
  return p;
}

int snd_pcm_open(snd_pcm_t** p, const char* name,
  snd_pcm_stream_t stream, int mode) {
  typedef int (*Open)(snd_pcm_t**, const char*, snd_pcm_stream_t, int);
  int code = ((Open)audio_test_symbol("snd_pcm_open"))(p, "null", stream, mode);
  if (code == 0) audio_test_unit = *p;
  return code;
}

snd_pcm_sframes_t snd_pcm_writei(snd_pcm_t* p, const void* data,
  snd_pcm_uframes_t frames) {
  typedef snd_pcm_sframes_t (*Write)(snd_pcm_t*, const void*, snd_pcm_uframes_t);
  const float* pcm = data;
  pthread_setspecific(audio_test_key, (void*)1);
  bool samples = false;
  for (u64 i = 0; i < frames; ++i) samples |= pcm[i * 2] != 0;
  if (p == audio_test_unit && (samples
    || (audio_test_mode >= 7 && audio_test_mode <= 14
      && atomic_load(&audio_test_fault)))) {
    u32 call = atomic_fetch_add(&audio_test_writes, 1);
    if (call > 100000) err_fail("audio test: unbounded writes");
    bool first = atomic_exchange(&audio_test_fault, 1) == 0;
    if (audio_test_mode == 1 && first) frames = audio_test_width;
    if (audio_test_mode == 15 && frames > 17) frames = 17;
    if ((audio_test_mode == 16 || audio_test_mode == 17) && call == 0) frames = 64;
    if (audio_test_mode == 16 && call == 1) return -EINTR;
    if (audio_test_mode == 17 && call == 1) return -EPIPE;
    if ((audio_test_mode == 2 && first) || audio_test_mode == 12) return -EINTR;
    if ((audio_test_mode == 3 && first) || audio_test_mode == 11) return -EPIPE;
    if ((audio_test_mode == 4 && first) || audio_test_mode == 7
      || audio_test_mode == 14) return -ESTRPIPE;
    if ((audio_test_mode == 5 && first) || audio_test_mode == 13) return -EAGAIN;
    if (audio_test_mode == 8 || audio_test_mode == 9) return -ENODEV;
    if (audio_test_mode == 10) return 0;
  }
  snd_pcm_sframes_t n = ((Write)audio_test_symbol("snd_pcm_writei"))(p, data, frames);
  if (n > 0 && p == audio_test_unit) {
    u32 count = atomic_load(&audio_test_frames);
    for (snd_pcm_sframes_t i = 0; i < n; ++i) {
      if (pcm[i * 2] == 0) {
        if (pcm[i * 2 + 1] != 0) atomic_store(&audio_test_bad, 1);
      } else {
        ++count;
        if (pcm[i * 2] != (float)count || pcm[i * 2 + 1] != -(float)count)
          atomic_store(&audio_test_bad, 1);
      }
    }
    atomic_store(&audio_test_frames, count);
  }
  if (!samples) {
    struct timespec t = { 0, 1000000 };
    nanosleep(&t, NULL);
  }
  return n;
}

int snd_pcm_recover(snd_pcm_t* p, int code, int silent) {
  typedef int (*Recover)(snd_pcm_t*, int, int);
  if (p == audio_test_unit) atomic_fetch_add(&audio_test_recovers, 1);
  return ((Recover)audio_test_symbol("snd_pcm_recover"))(p, code, silent);
}

int snd_pcm_resume(snd_pcm_t* p) {
  if (p == audio_test_unit) {
    u32 n = atomic_fetch_add(&audio_test_resumes, 1);
    return audio_test_mode == 7 || n < 2 ? -EAGAIN : -ENOSYS;
  }
  typedef int (*Resume)(snd_pcm_t*);
  return ((Resume)audio_test_symbol("snd_pcm_resume"))(p);
}

int snd_pcm_prepare(snd_pcm_t* p) {
  if (p == audio_test_unit && atomic_load(&audio_test_fault)
    && (audio_test_mode == 11 || audio_test_mode == 14)) return -EIO;
  typedef int (*Prepare)(snd_pcm_t*);
  return ((Prepare)audio_test_symbol("snd_pcm_prepare"))(p);
}

int snd_pcm_close(snd_pcm_t* p) {
  typedef int (*Close)(snd_pcm_t*);
  bool failed_close = p == audio_test_unit
    && (audio_test_mode == 8 || audio_test_mode == 9);
  if (p == audio_test_unit) {
    if (audio_test_mode >= 8 && audio_test_mode <= 14
      && !atomic_load(&audio_test_finished)) err_fail("audio test: close before pump exit");
    atomic_fetch_add(&audio_test_closes, 1);
    audio_test_unit = NULL;
  }
  int code = ((Close)audio_test_symbol("snd_pcm_close"))(p);
  if (failed_close) {
    if (code != 0 || !atomic_load(&audio_test_finished))
      err_fail("audio test: failed close did not release");
    puts("closed");
  }
  return code;
}

static void audio_test_read(int fd, char* out, size_t cap) {
  size_t n = 0;
  ssize_t got;
  while (n + 1 < cap && (got = read(fd, out + n, cap - n - 1)) > 0) n += got;
  out[n] = 0;
  close(fd);
}

static void audio_test_child(u32 mode) {
  int out[2], err[2];
  if (pipe(out) || pipe(err)) err_fail("audio test: pipe");
  io_sync();
  extern char** environ;
  size_t count = 0;
  while (environ[count] != NULL) ++count;
  char* vars[count + 2];
  size_t at = 0;
  for (size_t i = 0; i < count; ++i)
    if (strncmp(environ[i], "BEND_TEST_AUDIO_MODE=",
      sizeof "BEND_TEST_AUDIO_MODE=" - 1)) vars[at++] = environ[i];
  char value[64];
  snprintf(value, sizeof value, "BEND_TEST_AUDIO_MODE=%u", mode);
  vars[at++] = value;
  vars[at] = NULL;
  char* args[] = { "audio-test", "--threads", "1", "--gpu", "off", NULL };
  posix_spawn_file_actions_t actions;
  posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_addclose(&actions, out[0]);
  posix_spawn_file_actions_addclose(&actions, err[0]);
  posix_spawn_file_actions_adddup2(&actions, out[1], 1);
  posix_spawn_file_actions_adddup2(&actions, err[1], 2);
  posix_spawn_file_actions_addclose(&actions, out[1]);
  posix_spawn_file_actions_addclose(&actions, err[1]);
  pid_t pid;
  int spawn_code = posix_spawn(&pid, "/proc/self/exe", &actions, NULL, args, vars);
  posix_spawn_file_actions_destroy(&actions);
  if (spawn_code != 0) err_fail("audio test: spawn");
  close(out[1]); close(err[1]);
  int status = 0;
  u64 start = io_tick();
  while (waitpid(pid, &status, WNOHANG) == 0) {
    if (io_tick() - start > 2500000000ull) {
      kill(pid, SIGKILL);
      waitpid(pid, &status, 0);
      err_fail("audio test: child timeout");
    }
    struct timespec t = { 0, 1000000 };
    nanosleep(&t, NULL);
  }
  char text[128], errors[512];
  audio_test_read(out[0], text, sizeof text);
  audio_test_read(err[0], errors, sizeof errors);
  bool failed = mode >= 8 && mode <= 14;
  int code = mode == 8 || mode == 9 ? ENODEV
    : mode == 12 || mode == 13 ? ETIMEDOUT : EIO;
  char expected[160];
  snprintf(expected, sizeof expected, "bend: Audio: %s\n", snd_strerror(-code));
  if (!WIFEXITED(status) || WEXITSTATUS(status) != (failed ? 1 : 0)
    || strcmp(text, mode == 8 || mode == 9 ? "before\nclosed\n"
      : failed ? "before\n" : "")
    || (failed ? strcmp(errors, expected) != 0 : errors[0] != 0)) {
    fprintf(stderr, "audio test mode %u: status %d; stdout %s; stderr %s\n",
      mode, status, text, errors);
    err_fail("audio test: consumer failed");
  }
}
#endif

Term audio_test_mode_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  const char* mode = getenv("BEND_TEST_AUDIO_MODE");
  if (mode != NULL) return (u32)strtoul(mode, NULL, 10);
  for (u32 i = 1; i <= 17; ++i) audio_test_child(i);
#endif
  return 0;
}

Term audio_test_prepare_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  audio_test_mode = (u32)f[0];
  audio_test_width = (u32)f[1];
  const char* width = getenv("BEND_TEST_AUDIO_WIDTH");
  if (width != NULL && audio_test_mode == 1)
    audio_test_width = (u32)strtoul(width, NULL, 10);
  atomic_store(&audio_test_frames, 0);
  atomic_store(&audio_test_bad, 0);
  atomic_store(&audio_test_fault, 0);
  atomic_store(&audio_test_writes, 0);
  atomic_store(&audio_test_recovers, 0);
  atomic_store(&audio_test_resumes, 0);
  atomic_store(&audio_test_finished, 0);
  atomic_store(&audio_test_closes, 0);
#endif
  return term_pak(CID(Unit), 0);
}

Term audio_test_wait_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  u32 mode = (u32)f[0];
  u64 start = io_tick();
  while (mode == 7 ? atomic_load(&audio_test_resumes) < 1
    : mode >= 8 && mode <= 14 ? atomic_load(&audio_test_finished) == 0
    : atomic_load(&audio_test_frames) < 512) {
    if (atomic_load(&audio_test_bad)
      || io_tick() - start > (mode >= 8 && mode <= 14 ? 1500000000ull : 300000000ull)) {
      fprintf(stderr, "audio test: frames=%u writes=%u recoveries=%u resumes=%u\n",
        atomic_load(&audio_test_frames), atomic_load(&audio_test_writes),
        atomic_load(&audio_test_recovers), atomic_load(&audio_test_resumes));
      err_fail("audio test: pump made no progress");
    }
    struct timespec t = { 0, 100000 };
    nanosleep(&t, NULL);
  }
  if (atomic_load(&audio_test_bad)) err_fail("audio test: frame order or channels");
  if ((mode == 8 || mode == 9) && (atomic_load(&audio_test_writes) != 1
    || atomic_load(&audio_test_recovers) != 1)) err_fail("audio test: terminal retry");
  if ((mode == 12 || mode == 13) && atomic_load(&audio_test_writes) > 2000)
    err_fail("audio test: retries have no wait");
  if ((mode == 16 || mode == 17) && atomic_load(&audio_test_recovers) != 1)
    err_fail("audio test: partial write did not reach recovery");
#endif
  return term_pak(CID(Unit), 0);
}

Term audio_test_closed_run(Env e, Term* f, IoWork* w) {
#ifdef __linux__
  if (!atomic_load(&audio_test_finished) || atomic_load(&audio_test_closes) != 1)
    err_fail("audio test: close did not join and release");
  if (audio_test_mode != 7 && (atomic_load(&audio_test_frames) != 512
    || atomic_load(&audio_test_bad))) err_fail("audio test: final frames");
#endif
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) audio_test_use(void) {
#ifdef __linux__
  audio_test_library = snd_dlopen(NULL, RTLD_LAZY, NULL, 0);
  if (audio_test_library == NULL) err_fail("audio test: missing ALSA library");
  if (pthread_key_create(&audio_test_key, audio_test_exit))
    err_fail("audio test: thread key");
#endif
  io_eff(CID(AudioTest.mode), audio_test_mode_run);
  io_eff(CID(AudioTest.prepare), audio_test_prepare_run);
  io_eff(CID(AudioTest.wait), audio_test_wait_run);
  io_eff(CID(AudioTest.closed), audio_test_closed_run);
}
