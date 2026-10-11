// Chan
// ====

// Chan.new, send, recv, close and the try_ twins share this file: the rows
// live here, and each effect registers when the program uses it.

typedef struct {
  u32   gen;
  u32   next;
  u32   room;
  u32   size;
  u32   head;
  u32   live;
  u32   shut;
  Term* ring;
  IoWork* wait;
} ChanRow;

// A channel is Data: its handle is copied and may outlive the row, so it
// names the row by index and generation, a freed row waits on a list and
// comes back one generation up, and a stale copy finds no row (closed).
static ChanRow* chan_rows;
static u32      chan_len;
static u32      chan_idle = ~0u;

#define chan_some(e, v) io_box(e, CID(Some), v)
#define chan_none       term_pak(CID(None), 0)
#define chan_done(e)    io_done(e, term_pak(CID(Unit), 0))
#define chan_wait(e, item) io_box(e, CID(Wait), \
  (item) == TERM_HOLE ? term_pak(CID(Unit), 0) : (item))

static Term chan_open(u32 room) {
  u32 i = chan_idle;
  if (i != ~0u) {
    chan_idle = chan_rows[i].next;
  } else {
    if (chan_len == 1u << 24) {
      err_fail("more than 16777216 channels at once");
    }
    if ((chan_len & (chan_len - 1)) == 0) {
      chan_rows = io_mem(realloc(chan_rows,
        (chan_len == 0 ? 1 : 2 * chan_len) * sizeof(ChanRow)));
    }
    i = chan_len;
    chan_len += 1;
    chan_rows[i].gen = 0;
  }
  ChanRow* row = &chan_rows[i];
  *row = (ChanRow){ .gen = row->gen + 1, .room = room, .live = 1,
    .ring = room == 0 ? NULL : io_mem(malloc(room * sizeof(Term))) };
  return io_hand(((u64)row->gen << 24) | i);
}

static ChanRow* chan_at(Term t) {
  u64      v   = io_hand_v(t);
  u32      i   = (u32)v & 0xFFFFFF;
  ChanRow* row = i < chan_len ? &chan_rows[i] : NULL;
  return row != NULL && row->live && row->gen == (u32)(v >> 24) ? row : NULL;
}

// Cuts w from the queue q (a ring whose tail is *q), a row's waiters.
static void chan_cut(IoWork** q, IoWork* w) {
  IoWork* p = *q;
  while (p->next != w) {
    p = p->next;
  }
  p->next = w->next;
  if (*q == w) {
    *q = p != w ? p : NULL;
  }
}

// A try_ waiter's timer (hand: the waiter; word: its row), parked on
// io_park while the waiter sits on the row; the first to come cuts the other.
static Term chan_late(Env e, IoWork* t) {
  IoWork* w = (IoWork*)t->hand;
  chan_cut(&chan_rows[t->word].wait, w);
  w->item = chan_wait(e, w->item);
  io_push(&io_runs, w);
  free(t);
  return IO_PARK;
}

// Parks the effect's activation on row with item: a sent value, or
// TERM_HOLE for a receiver. made is its timer if try_, else 0.
static Term chan_park(ChanRow* row, IoWork* w, Term item, u64 at) {
  w->item = item;
  w->made = 0;
  if (at != 0) {
    IoWork* t = io_mem(calloc(1, sizeof(IoWork)));
    t->hand = (intptr_t)w;
    t->word = (u32)(row - chan_rows);
    t->time = at;
    t->pack = chan_late;
    io_park_add(t);
    w->made = (intptr_t)t;
  }
  io_push(&row->wait, w);
  return IO_PARK;
}

// Wakes the first waiter with x, as Ready{x} if try_ (cutting its timer).
static Term chan_wake(Env e, ChanRow* row, Term x) {
  IoWork* a    = io_pop(&row->wait);
  Term    item = a->item;
  if (a->made != 0) {
    io_park_cut((IoWork*)a->made);
    free((IoWork*)a->made);
    x = io_box(e, CID(Ready), x);
  }
  a->item = x;
  io_push(&io_runs, a);
  return item;
}

static Term chan_take(Env e, ChanRow* row) {
  Term v = row->ring[row->head];
  row->head = (row->head + 1) % row->room;
  row->size -= 1;
  if (row->wait != NULL) {
    Term item = chan_wake(e, row, chan_done(e));
    row->ring[(row->head + row->size) % row->room] = item;
    row->size += 1;
  }
  return v;
}

static void chan_free(ChanRow* row) {
  free(row->ring);
  row->live = 0;
  row->next = chan_idle;
  chan_idle = (u32)(row - chan_rows);
}

// Parked receivers answer None{}, parked senders Fail{value}.
static void chan_shut(Env e, ChanRow* row) {
  row->shut = 1;
  while (row->wait != NULL) {
    Term item = row->wait->next->item;
    chan_wake(e, row, item == TERM_HOLE ? chan_none
      : io_box(e, CID(Fail), item));
  }
  if (row->size == 0) {
    chan_free(row);
  }
}

// Sends without waiting: Done{}, Fail{v} if closed, IO_PARK if it would.
static Term chan_put(Env e, ChanRow* row, Term v) {
  if (row == NULL || row->shut) {
    return io_box(e, CID(Fail), v);
  }
  if (row->wait != NULL && row->wait->next->item == TERM_HOLE) {
    chan_wake(e, row, chan_some(e, v));
    return chan_done(e);
  }
  if (row->size < row->room) {
    row->ring[(row->head + row->size) % row->room] = v;
    row->size += 1;
    return chan_done(e);
  }
  return IO_PARK;
}

// Receives without waiting: Some{v}, None{} at the end, IO_PARK if it would.
static Term chan_get(Env e, ChanRow* row) {
  if (row == NULL) {
    return chan_none;
  }
  if (row->size > 0) {
    Term v = chan_take(e, row);
    if (row->shut && row->size == 0) {
      chan_free(row);
    }
    return chan_some(e, v);
  }
  if (row->wait != NULL && row->wait->next->item != TERM_HOLE) {
    return chan_some(e, chan_wake(e, row, chan_done(e)));
  }
  if (row->shut) {
    chan_free(row);
    return chan_none;
  }
  return IO_PARK;
}

// Ready{x} now, Wait{rest} now if ms is 0, else parks until a wake or ms.
static Term chan_try(Env e, ChanRow* row, IoWork* w, Term x, Term item,
  u32 ms) {
  if (x != IO_PARK) {
    return io_box(e, CID(Ready), x);
  }
  if (ms == 0) {
    return chan_wait(e, item);
  }
  return chan_park(row, w, item, io_until(ms));
}

#ifdef CID(Chan.new)

Term chan_new_run(Env e, Term* f, IoWork* w) {
  return chan_open((u32)f[0]);
}

static void __attribute__((constructor)) chan_new_use(void) {
  io_eff(CID(Chan.new), chan_new_run);
}

#endif

#ifdef CID(Chan.send)

Term chan_send_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(f[0]);
  Term     x   = chan_put(e, row, f[1]);
  return x != IO_PARK ? x : chan_park(row, w, f[1], 0);
}

static void __attribute__((constructor)) chan_send_use(void) {
  io_eff(CID(Chan.send), chan_send_run);
}

#endif

#ifdef CID(Chan.recv)

Term chan_recv_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(f[0]);
  Term     x   = chan_get(e, row);
  return x != IO_PARK ? x : chan_park(row, w, TERM_HOLE, 0);
}

static void __attribute__((constructor)) chan_recv_use(void) {
  io_eff(CID(Chan.recv), chan_recv_run);
}

#endif

#ifdef CID(Chan.try_send)

Term chan_try_send_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(f[0]);
  return chan_try(e, row, w, chan_put(e, row, f[1]), f[1], (u32)f[2]);
}

static void __attribute__((constructor)) chan_try_send_use(void) {
  io_eff(CID(Chan.try_send), chan_try_send_run);
}

#endif

#ifdef CID(Chan.try_recv)

Term chan_try_recv_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(f[0]);
  return chan_try(e, row, w, chan_get(e, row), TERM_HOLE, (u32)f[1]);
}

static void __attribute__((constructor)) chan_try_recv_use(void) {
  io_eff(CID(Chan.try_recv), chan_try_recv_run);
}

#endif

#ifdef CID(Chan.close)

Term chan_close_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(f[0]);
  if (row != NULL && !row->shut) {
    chan_shut(e, row);
  }
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) chan_close_use(void) {
  io_eff(CID(Chan.close), chan_close_run);
}

#endif
