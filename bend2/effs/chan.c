// Chan
// ====

// Chan.new, send, recv, close and the try_ twins share this file: their
// rows live in the native heap, and each used effect registers itself.

typedef struct {
  u64 room;
  u64 size;
  Term items;
  u64 shut;
  Term wait;
  Term tail;
} ChanRow;

// The opaque handle owns an ARR row and a FIFO of two-Term ARR nodes.
// Ordinary term_keep/drop tracks copies and releases unread buffered Terms.
// Parked operations own a handle until their wake or deadline.

#define chan_some(e, v) io_box(e, CID(Some), v)
#define chan_none       term_pak(CID(None), 0)
#define chan_done(e)    io_done(e, term_pak(CID(Unit), 0))
#define chan_wait(e, item) io_box(e, CID(Wait), \
  (item) == TERM_HOLE ? term_pak(CID(Unit), 0) : (item))

static Term chan_open(Env e, u32 room) {
  u32 cls = cls_fit((u32)(sizeof(ChanRow) / sizeof(Term)));
  BLK_ALLOC(loc, cls)
  memset(e.mem + loc, 0, (1ull << cls) * sizeof(Term));
  ChanRow* row = (ChanRow*)(e.mem + loc);
  row->room = room;
  // Data: born shared, so a node that holds a copy may itself be shared.
  return rfc_wrap(e, term_blk(true, cls, loc), 1);
}

static ChanRow* chan_at(Env e, Term t) {
  return (ChanRow*)(e.mem + term_peek(e.mem, t));
}

static void chan_link(Env e, ChanRow* row, Term node) {
  if (row->size == 0) {
    row->items = node;
  } else {
    e.mem[io_hand_v(row->tail) + 1] = node;
  }
  row->tail = io_hand(term_loc(node));
  row->size += 1;
}

static IoWork* chan_tail(ChanRow* row) {
  return (IoWork*)(uintptr_t)io_hand_v(row->wait);
}

static void chan_queue(ChanRow* row, IoWork* tail) {
  row->wait = tail == NULL ? 0 : io_hand((u64)(uintptr_t)tail);
}

// Removes w from a row's doubly linked waiter ring. *q is the tail.
static void chan_cut(IoWork** q, IoWork* w) {
  IoWork* p = w->prev;
  p->next = w->next;
  w->next->prev = p;
  if (*q == w) {
    *q = p != w ? p : NULL;
  }
}

// A try_ waiter's timer (hand: the waiter; data: its owned row), parked on
// io_park while the waiter sits on the row; the first to come cuts the other.
static Term chan_late(Env e, IoWork* t) {
  IoWork* w = (IoWork*)t->hand;
  ChanRow* row = (ChanRow*)t->data;
  IoWork* queue = chan_tail(row);
  chan_cut(&queue, w);
  chan_queue(row, queue);
  w->item = chan_wait(e, w->item);
  io_push(&io_runs, w);
  term_drop(e, (Term)w->hand);
  free(t);
  return IO_PARK;
}

// Parks the effect's activation on row with item: a sent value, or
// TERM_HOLE for a receiver. made is its timer if try_, else 0.
static Term chan_park(ChanRow* row, IoWork* w, Term hand, Term item, u64 at) {
  w->item = item;
  w->hand = (intptr_t)hand;
  w->made = 0;
  if (at != 0) {
    IoWork* t = io_mem(calloc(1, sizeof(IoWork)));
    t->hand = (intptr_t)w;
    t->data = (char*)row;
    t->time = at;
    t->pack = chan_late;
    io_park_add(t);
    w->made = (intptr_t)t;
  }
  IoWork* queue = chan_tail(row);
  w->prev = queue == NULL ? w : queue;
  io_push(&queue, w);
  w->next->prev = w;
  chan_queue(row, queue);
  return IO_PARK;
}

// Wakes the first waiter with x, as Ready{x} if try_ (cutting its timer).
static Term chan_wake(Env e, ChanRow* row, Term x) {
  IoWork* queue = chan_tail(row);
  IoWork* a = io_pop(&queue);
  if (queue != NULL) {
    queue->next->prev = queue;
  }
  chan_queue(row, queue);
  Term    item = a->item;
  if (a->made != 0) {
    io_park_cut((IoWork*)a->made);
    free((IoWork*)a->made);
    x = io_box(e, CID(Ready), x);
  }
  a->item = x;
  io_push(&io_runs, a);
  term_drop(e, (Term)a->hand);
  return item;
}

static Term chan_take(Env e, ChanRow* row) {
  Term node = row->items;
  u64 loc = term_loc(node);
  Term v = e.mem[loc];
  row->items = e.mem[loc + 1];
  row->size -= 1;
  if (row->size == 0) {
    row->tail = 0;
  }
  if (row->wait != 0) {
    e.mem[loc] = chan_wake(e, row, chan_done(e));
    e.mem[loc + 1] = 0;
    chan_link(e, row, node);
  } else {
    heap_free(e, 1, loc);
  }
  return v;
}

// Parked receivers answer None{}, parked senders Fail{value}.
static void chan_shut(Env e, ChanRow* row) {
  row->shut = 1;
  while (row->wait != 0) {
    Term item = chan_tail(row)->next->item;
    chan_wake(e, row, item == TERM_HOLE ? chan_none
      : io_box(e, CID(Fail), item));
  }
}

// Sends without waiting: Done{}, Fail{v} if closed, IO_PARK if it would.
static Term chan_put(Env e, ChanRow* row, Term v) {
  if (row->shut) {
    return io_box(e, CID(Fail), v);
  }
  if (row->wait != 0 && chan_tail(row)->next->item == TERM_HOLE) {
    chan_wake(e, row, chan_some(e, v));
    return chan_done(e);
  }
  if (row->size < row->room) {
    BLK_ALLOC(loc, 1)
    e.mem[loc] = v;
    e.mem[loc + 1] = 0;
    chan_link(e, row, term_blk(true, 1, loc));
    return chan_done(e);
  }
  return IO_PARK;
}

// Receives without waiting: Some{v}, None{} at the end, IO_PARK if it would.
static Term chan_get(Env e, ChanRow* row) {
  if (row->size > 0) {
    Term v = chan_take(e, row);
    return chan_some(e, v);
  }
  if (row->wait != 0 && chan_tail(row)->next->item != TERM_HOLE) {
    return chan_some(e, chan_wake(e, row, chan_done(e)));
  }
  if (row->shut) {
    return chan_none;
  }
  return IO_PARK;
}

// Ready{x} now, Wait{rest} now if ms is 0, else parks until a wake or ms.
static Term chan_try(Env e, ChanRow* row, IoWork* w, Term hand, Term x,
  Term item, u32 ms) {
  if (x != IO_PARK) {
    term_drop(e, hand);
    return io_box(e, CID(Ready), x);
  }
  if (ms == 0) {
    term_drop(e, hand);
    return chan_wait(e, item);
  }
  return chan_park(row, w, hand, item, io_until(ms));
}

#ifdef CID(Chan.new)

Term chan_new_run(Env e, Term* f, IoWork* w) {
  return chan_open(e, (u32)f[0]);
}

static void __attribute__((constructor)) chan_new_use(void) {
  io_eff(CID(Chan.new), chan_new_run);
}

#endif

#ifdef CID(Chan.send)

Term chan_send_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(e, f[0]);
  Term     x   = chan_put(e, row, f[1]);
  if (x == IO_PARK) {
    return chan_park(row, w, f[0], f[1], 0);
  }
  term_drop(e, f[0]);
  return x;
}

static void __attribute__((constructor)) chan_send_use(void) {
  io_eff(CID(Chan.send), chan_send_run);
}

#endif

#ifdef CID(Chan.recv)

Term chan_recv_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(e, f[0]);
  Term     x   = chan_get(e, row);
  if (x == IO_PARK) {
    return chan_park(row, w, f[0], TERM_HOLE, 0);
  }
  term_drop(e, f[0]);
  return x;
}

static void __attribute__((constructor)) chan_recv_use(void) {
  io_eff(CID(Chan.recv), chan_recv_run);
}

#endif

#ifdef CID(Chan.try_send)

Term chan_try_send_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(e, f[0]);
  return chan_try(e, row, w, f[0], chan_put(e, row, f[1]), f[1], (u32)f[2]);
}

static void __attribute__((constructor)) chan_try_send_use(void) {
  io_eff(CID(Chan.try_send), chan_try_send_run);
}

#endif

#ifdef CID(Chan.try_recv)

Term chan_try_recv_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(e, f[0]);
  return chan_try(e, row, w, f[0], chan_get(e, row), TERM_HOLE, (u32)f[1]);
}

static void __attribute__((constructor)) chan_try_recv_use(void) {
  io_eff(CID(Chan.try_recv), chan_try_recv_run);
}

#endif

#ifdef CID(Chan.close)

Term chan_close_run(Env e, Term* f, IoWork* w) {
  ChanRow* row = chan_at(e, f[0]);
  if (!row->shut) {
    chan_shut(e, row);
  }
  term_drop(e, f[0]);
  return term_pak(CID(Unit), 0);
}

static void __attribute__((constructor)) chan_close_use(void) {
  io_eff(CID(Chan.close), chan_close_run);
}

#endif
