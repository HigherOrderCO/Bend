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

// Wakes the first waiter with x, as Ready{x} if try_ (cutting its timer).
function chan_wake(row, x) {
  const w = row.wait.shift();
  if (w.timer !== undefined) {
    io_park_cut(w.timer);
    x = { $: CID(Ready), value: x };
  }
  io_push(w.cont, x, false);
  return w.item;
}

function chan_take(row) {
  const v = row.ring.shift();
  if (row.wait.length > 0) {
    row.ring.push(chan_wake(row, chan_done()));
  }
  return v;
}

// A handle is the row (a stale copy keeps it, shut).
function chan_shut(row) {
  row.shut = true;
  while (row.wait.length > 0) {
    const item = row.wait[0].item;
    chan_wake(row, item === CHAN_RECV ? { $: CID(None) }
      : { $: CID(Fail), error: item });
  }
}

// Parks k on row with item; with ms, a timer too, whose late answers Wait:
// the first of the two to come cuts the other.
function chan_park(row, k, item, ms) {
  const w = { cont: k, item: item };
  if (ms !== undefined) {
    w.timer = io_park_on(undefined, false, k, () => {
      row.wait.splice(row.wait.indexOf(w), 1);
      return chan_wait(item);
    }, io_until(ms));
  }
  row.wait.push(w);
  return undefined;
}

// Sends without waiting: Done{}, Fail{v} if closed, undefined if it would.
function chan_put(row, value) {
  if (row.shut) {
    return { $: CID(Fail), error: value };
  }
  if (row.wait.length > 0 && row.wait[0].item === CHAN_RECV) {
    chan_wake(row, { $: CID(Some), value: value });
    return chan_done();
  }
  if (row.ring.length < row.room) {
    row.ring.push(value);
    return chan_done();
  }
  return undefined;
}

// Receives without waiting: Some{v}, None{} at the end, undefined if not.
function chan_get(row) {
  if (row.ring.length > 0) {
    return { $: CID(Some), value: chan_take(row) };
  }
  if (row.wait.length > 0 && row.wait[0].item !== CHAN_RECV) {
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
  return { room: Number(room), ring: [], wait: [], shut: false };
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
