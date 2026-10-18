// Chan
// ====

// Chan.new, send, recv, close and the try_ twins share this file.

// A parked receiver holds CHAN_RECV: the C lane parks TERM_HOLE, and a
// program can make neither. A sent value may be null (an erased proof).
const CHAN_RECV = Symbol();

function chan_done() {
  return { $: CID(Done), value: { $: CID(Unit) } };
}

function chan_wait(item) {
  return { $: CID(Wait), rest: item === CHAN_RECV ? { $: CID(Unit) } : item };
}

function chan_cut(row, w) {
  if (w.prev === null) row.wait = w.next;
  else w.prev.next = w.next;
  if (w.next === null) row.last = w.prev;
  else w.next.prev = w.prev;
}

// Wakes the first waiter with x, as Ready{x} if try_ (cutting its timer).
function chan_wake(row, x) {
  const w = row.wait;
  chan_cut(row, w);
  if (w.late !== undefined) {
    const ws = io_active.waits;
    ws.splice(ws.findIndex((t) => t.more === w.late), 1);
    x = { $: CID(Ready), value: x };
  }
  io_push(w.cont, x, false);
  return w.item;
}

function chan_buffer(row, value) {
  if (row.fill === row.ring.length) {
    const old = row.ring;
    row.ring = new Array(Math.min(row.room, Math.max(8, 2 * old.length)));
    for (let i = 0; i < row.fill; i += 1) {
      row.ring[i] = old[(row.head + i) % old.length];
    }
    row.head = 0;
  }
  row.ring[(row.head + row.fill) % row.ring.length] = value;
  row.fill += 1;
}

function chan_take(row) {
  const v = row.ring[row.head];
  row.ring[row.head] = undefined;
  row.head = (row.head + 1) % row.ring.length;
  row.fill -= 1;
  if (row.wait !== null) {
    chan_buffer(row, chan_wake(row, chan_done()));
  }
  return v;
}

// A handle is the row (a stale copy keeps it, shut).
function chan_shut(row) {
  row.shut = true;
  while (row.wait !== null) {
    const item = row.wait.item;
    chan_wake(row, item === CHAN_RECV ? { $: CID(None) }
      : { $: CID(Fail), error: item });
  }
}

// Parks k on row with item; with ms, a timer too, whose late answers Wait:
// the first of the two to come cuts the other.
function chan_park(row, k, item, ms) {
  const w = { cont: k, item: item, prev: row.last, next: null };
  if (ms !== undefined) {
    w.late = () => {
      chan_cut(row, w);
      return chan_wait(item);
    };
    io_park_on(undefined, false, k, w.late, io_until(ms));
  }
  if (row.last === null) row.wait = w;
  else row.last.next = w;
  row.last = w;
  return undefined;
}

// Sends without waiting: Done{}, Fail{v} if closed, undefined if it would.
function chan_put(row, value) {
  if (row.shut) {
    return { $: CID(Fail), error: value };
  }
  if (row.wait !== null && row.wait.item === CHAN_RECV) {
    chan_wake(row, { $: CID(Some), value: value });
    return chan_done();
  }
  if (row.fill < row.room) {
    chan_buffer(row, value);
    return chan_done();
  }
  return undefined;
}

// Receives without waiting: Some{v}, None{} at the end, undefined if not.
function chan_get(row) {
  if (row.fill > 0) {
    return { $: CID(Some), value: chan_take(row) };
  }
  if (row.wait !== null && row.wait.item !== CHAN_RECV) {
    return { $: CID(Some), value: chan_wake(row, chan_done()) };
  }
  if (row.shut) {
    return { $: CID(None) };
  }
  return undefined;
}

// Ready{x} now, Wait{rest} now if ms is 0, else parks until a wake or ms.
function chan_try(row, x, item, ms, k) {
  if (x !== undefined) {
    return { $: CID(Ready), value: x };
  }
  if (Number(ms) === 0) {
    return chan_wait(item);
  }
  return chan_park(row, k, item, Number(ms));
}

function chan_new(room) {
  return { room: Number(room), ring: [], head: 0, fill: 0,
    wait: null, last: null, shut: false };
}

function chan_send(row, value, k) {
  const x = chan_put(row, value);
  return x !== undefined ? x : chan_park(row, k, value);
}

function chan_recv(row, k) {
  const x = chan_get(row);
  return x !== undefined ? x : chan_park(row, k, CHAN_RECV);
}

function chan_try_send(row, value, ms, k) {
  return chan_try(row, chan_put(row, value), value, ms, k);
}

function chan_try_recv(row, ms, k) {
  return chan_try(row, chan_get(row), CHAN_RECV, ms, k);
}

function chan_close(row) {
  if (!row.shut) {
    chan_shut(row);
  }
  return { $: CID(Unit) };
}

io_eff(CID(Chan.new), chan_new);
io_eff(CID(Chan.send), chan_send);
io_eff(CID(Chan.recv), chan_recv);
io_eff(CID(Chan.try_send), chan_try_send);
io_eff(CID(Chan.try_recv), chan_try_recv);
io_eff(CID(Chan.close), chan_close);
