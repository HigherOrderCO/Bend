// IO
// ==

// IO.eval(f) applies f to Unit on one eval thread, so the loop keeps
// serving fds, timers and channels while a long pure computation runs.
// The thread owns the worker pool through io_owner and allocates from the
// spare row ALC[CUBE_T] with its own stack; a loop step that finds the
// pool owned evaluates sequentially and answers through io_alt. Evals
// queue for that thread, never for the IO helpers, so a long one holds
// no helper that a file or socket effect needs.

static pthread_mutex_t io_eval_gate = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t  io_eval_bell = PTHREAD_COND_INITIALIZER;
static IoWork*         io_eval_jobs;

static void* io_eval_help(void* arg) {
  io_stk = pool_stack();
  io_alc = CUBE_T;
  for (;;) {
    pthread_mutex_lock(&io_eval_gate);
    while (io_eval_jobs == NULL) {
      pthread_cond_wait(&io_eval_bell, &io_eval_gate);
    }
    IoWork* w = io_pop(&io_eval_jobs);
    pthread_mutex_unlock(&io_eval_gate);
    pthread_mutex_lock(&io_owner);
    Env e = { CORPUS, ALC[io_alc] };
    u64 ap = task_node(e, FID(Clo~apply), TERM_HOLE, 0, 0);
    e.mem[ap]     = w->item;
    e.mem[ap + 1] = term_pak(CID(Unit), 0);
    w->item = corpus_eval(CORPUS, term_tsk(FID(Clo~apply), ap));
    pthread_mutex_unlock(&io_owner);
    while (write(io_wake_fd[1], &w, sizeof w) != sizeof w) {
    }
  }
}

static Term io_eval_pack(Env e, IoWork* w) {
  return w->item;
}

Term io_eval_run(Env e, Term* f, IoWork* w) {
  static bool live;
  if (!live) {
    pthread_t tid;
    if (pthread_create(&tid, NULL, io_eval_help, NULL)) {
      err_fail("pthread_create");
    }
    pthread_detach(tid);
    live = true;
  }
  w->item  = f[0];
  w->pack  = io_eval_pack;
  io_busy += 1;
  pthread_mutex_lock(&io_eval_gate);
  io_push(&io_eval_jobs, w);
  pthread_cond_signal(&io_eval_bell);
  pthread_mutex_unlock(&io_eval_gate);
  return IO_PARK;
}

static void __attribute__((constructor)) io_eval_use(void) {
  io_eff(CID(IO.eval), io_eval_run);
}
