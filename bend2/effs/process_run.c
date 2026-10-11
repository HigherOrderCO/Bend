// Process
// =======

#include <spawn.h>
#include <sys/wait.h>
#ifdef __APPLE__
#include <sys/event.h>
#else
#include <sys/syscall.h>
#endif

extern char** environ;

typedef struct {
  char** argv;
  u32 argc;
  char* input;
  u64 input_len;
  char* out;
  char* err;
  u64 out_len;
  u64 err_len;
  u64 out_cap;
  u64 err_cap;
  u32 max;
  u32 timeout;
  u32 status;
  u32 code;
} ProcessCall;

static void process_free(ProcessCall* p) {
  for (u32 i = 0; i < p->argc; i += 1) {
    free(p->argv[i]);
  }
  free(p->argv);
  free(p->input);
  free(p->out);
  free(p->err);
  free(p);
}

static void process_append(ProcessCall* p, bool error, const char* data,
  u64 size) {
  if (size > (u64)p->max - p->out_len - p->err_len) {
    p->code = EFBIG;
    return;
  }
  char** buf = error ? &p->err : &p->out;
  u64* len  = error ? &p->err_len : &p->out_len;
  u64* cap  = error ? &p->err_cap : &p->out_cap;
  u64 need  = *len + size;
  if (need > *cap) {
    u64 next = *cap ? *cap : 4096;
    while (next < need) {
      next *= 2;
    }
    if (next > p->max) {
      next = p->max;
    }
    *buf = io_mem(realloc(*buf, next + 1));
    *cap = next;
  }
  memcpy(*buf + *len, data, size);
  *len = need;
}

// A descriptor that polls readable once the child exits.
static int process_exitfd(pid_t child) {
#ifdef __APPLE__
  int kq = kqueue();
  struct kevent ev;
  EV_SET(&ev, child, EVFILT_PROC, EV_ADD | EV_ONESHOT, NOTE_EXIT, 0, NULL);
  if (kq >= 0 && kevent(kq, &ev, 1, NULL, 0, NULL) != 0) {
    close(kq);
    kq = -1;
  }
  return kq;
#elif defined(SYS_pidfd_open)
  return (int)syscall(SYS_pidfd_open, child, 0);
#else
  return -1;
#endif
}

static int process_nonblock(int fd) {
  int flags = fcntl(fd, F_GETFL);
  return flags < 0 ? -1 : fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

static int process_pipe(int fds[2]) {
  if (pipe(fds) != 0) {
    return -1;
  }
  for (int i = 0; i < 2; i += 1) {
    if (fds[i] < 3) {
      int moved = fcntl(fds[i], F_DUPFD, 3);
      if (moved < 0) {
        return -1;
      }
      close(fds[i]);
      fds[i] = moved;
    }
    if (fcntl(fds[i], F_SETFD, FD_CLOEXEC) != 0) {
      return -1;
    }
  }
  return 0;
}

static void process_call(IoWork* w) {
  ProcessCall* p = (ProcessCall*)w->data;
  int pipes[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};
  pid_t child = -1;
  int status = 0;
  int exitfd = -1;
  for (int i = 0; i < 3; i += 1) {
    if (process_pipe(pipes[i]) != 0) {
      p->code = errno;
      goto done;
    }
  }
  if (process_nonblock(pipes[0][1]) != 0
    || process_nonblock(pipes[1][0]) != 0
    || process_nonblock(pipes[2][0]) != 0) {
    p->code = errno;
    goto done;
  }
  posix_spawn_file_actions_t actions;
  p->code = posix_spawn_file_actions_init(&actions);
  if (p->code != 0) {
    goto done;
  }
  p->code = posix_spawn_file_actions_adddup2(&actions, pipes[0][0], 0);
  if (p->code == 0) {
    p->code = posix_spawn_file_actions_adddup2(&actions, pipes[1][1], 1);
  }
  if (p->code == 0) {
    p->code = posix_spawn_file_actions_adddup2(&actions, pipes[2][1], 2);
  }
  posix_spawnattr_t attr;
  posix_spawnattr_t* attrp = NULL;
  if (p->code == 0) {
    p->code = posix_spawnattr_init(&attr);
    if (p->code == 0) {
      attrp = &attr;
      sigset_t defaults;
      sigemptyset(&defaults);
      sigaddset(&defaults, SIGPIPE);
      p->code = posix_spawnattr_setsigdefault(&attr, &defaults);
      if (p->code == 0) {
        short flags = POSIX_SPAWN_SETSIGDEF;
#ifdef __APPLE__
        flags |= POSIX_SPAWN_CLOEXEC_DEFAULT;
#endif
        p->code = posix_spawnattr_setflags(&attr, flags);
      }
    }
  }
#ifndef __APPLE__
  if (p->code == 0) {
    p->code = posix_spawn_file_actions_addclosefrom_np(&actions, 3);
  }
#endif
  if (p->code == 0) {
    p->code = posix_spawnp(&child, p->argv[0], &actions, attrp, p->argv,
      environ);
  }
  if (attrp != NULL) {
    posix_spawnattr_destroy(attrp);
  }
  posix_spawn_file_actions_destroy(&actions);
  if (p->code != 0) {
    child = -1;
    goto done;
  }
  close(pipes[0][0]); pipes[0][0] = -1;
  close(pipes[1][1]); pipes[1][1] = -1;
  close(pipes[2][1]); pipes[2][1] = -1;
  if (p->input_len == 0) {
    close(pipes[0][1]); pipes[0][1] = -1;
  }
  exitfd = process_exitfd(child);
  u64 deadline = io_tick() + (u64)p->timeout * 1000000ull;
  u64 written  = 0;
  while (p->code == 0) {
    bool reading = pipes[1][0] >= 0 || pipes[2][0] >= 0;
    pid_t got = reading ? 0 : waitpid(child, &status, WNOHANG);
    if (got == child) {
      child = -1;
      break;
    }
    if (got < 0 && errno != EINTR) {
      p->code = errno;
      break;
    }
    u64 now = io_tick();
    if (now >= deadline) {
      p->code = ETIMEDOUT;
      break;
    }
    struct pollfd fds[4] = {
      {pipes[0][1], POLLOUT, 0},
      {pipes[1][0], POLLIN, 0},
      {pipes[2][0], POLLIN, 0},
      {reading ? -1 : exitfd, POLLIN, 0}
    };
    u64 left = (deadline - now + 999999ull) / 1000000ull;
    u64 most = reading || exitfd >= 0 ? 1000000 : 50;
    int ready = poll(fds, 4, (int)(left > most ? most : left));
    if (ready < 0) {
      if (errno == EINTR) {
        continue;
      }
      p->code = errno;
      break;
    }
    for (int i = 0; i < 3 && p->code == 0; i += 1) {
      if (fds[i].revents == 0) {
        continue;
      }
      if (i == 0) {
        u64 remain = p->input_len - written;
        size_t size = remain < 8192 ? (size_t)remain : 8192;
        ssize_t n = write(pipes[0][1], p->input + written, size);
        if (n > 0) {
          written += (u64)n;
        } else if (n < 0 && errno != EAGAIN && errno != EINTR
          && errno != EPIPE) {
          p->code = errno;
        }
        if (written == p->input_len || (n < 0 && errno == EPIPE)) {
          close(pipes[0][1]); pipes[0][1] = -1;
        }
      } else {
        char buf[8192];
        ssize_t n = read(pipes[i][0], buf, sizeof buf);
        if (n > 0) {
          process_append(p, i == 2, buf, (u64)n);
        } else if (n == 0) {
          close(pipes[i][0]); pipes[i][0] = -1;
        } else if (errno != EAGAIN && errno != EINTR) {
          p->code = errno;
        }
      }
    }
  }
  if (child >= 0) {
    if (p->code != 0) {
      kill(child, SIGKILL);
    }
    while (waitpid(child, &status, 0) < 0 && errno == EINTR) {
    }
  }
  if (p->code == 0) {
    p->status = WIFEXITED(status) ? (u32)WEXITSTATUS(status)
      : WIFSIGNALED(status) ? 128 + (u32)WTERMSIG(status) : 1;
  }
done:
  if (exitfd >= 0) {
    close(exitfd);
  }
  for (int i = 0; i < 3; i += 1) {
    for (int j = 0; j < 2; j += 1) {
      if (pipes[i][j] >= 0) {
        close(pipes[i][j]);
      }
    }
  }
}

static Term process_pack(Env e, IoWork* w) {
  ProcessCall* p = (ProcessCall*)w->data;
  Term result;
  if (p->code != 0) {
    result = io_fail(e, p->code, NULL);
  } else {
    Term out = io_str(e, p->out == NULL ? "" : p->out, p->out_len);
    Term err = io_str(e, p->err == NULL ? "" : p->err, p->err_len);
    result = io_done(e, io_tup(e, p->status, io_tup(e, out, err)));
  }
  process_free(p);
  return result;
}

Term process_run_run(Env e, Term* f, IoWork* w) {
  ProcessCall* p = io_mem(calloc(1, sizeof(ProcessCall)));
  u64 len = 0;
  char* command = io_cstr(e, f[0], &len);
  p->code = io_nul(command, len) ? EINVAL : 0;
  u32 capacity = 8;
  p->argv = io_mem(calloc(capacity, sizeof(char*)));
  p->argv[p->argc++] = command;
  Term xs = f[1];
  while (term_aux(xs) == CID_CON) {
    Term pair[2];
    spare_free(e, cls_fit(2), ctr_take(e, xs, 2, pair));
    char* arg = io_cstr(e, pair[0], &len);
    if (io_nul(arg, len)) {
      p->code = EINVAL;
    }
    if (p->argc + 1 >= capacity) {
      capacity *= 2;
      p->argv = io_mem(realloc(p->argv, capacity * sizeof(char*)));
    }
    p->argv[p->argc++] = arg;
    xs = pair[1];
  }
  p->argv[p->argc] = NULL;
  p->input = io_cstr(e, f[2], &p->input_len);
  p->max = (u32)f[3];
  p->timeout = (u32)f[4];
  if (p->max == 0 || p->timeout == 0) {
    p->code = EINVAL;
  }
  w->data = (char*)p;
  return p->code != 0 ? process_pack(e, w)
    : io_work(w, process_call, process_pack);
}

static void __attribute__((constructor)) process_run_use(void) {
  io_eff(CID(Process.run), process_run_run);
}
