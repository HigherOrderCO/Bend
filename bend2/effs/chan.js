// bend-ids 1
// CID #Done BEND_CID_Done
// CID #Unit BEND_CID_Unit
// CID #Wait BEND_CID_Wait
// CID #Ready BEND_CID_Ready
// CID #None BEND_CID_None
// CID #Fail BEND_CID_Fail
// CID #Some BEND_CID_Some
// CID :Chan.new BEND_CID_Chan_new
// CID :Chan.send BEND_CID_Chan_send
// CID :Chan.recv BEND_CID_Chan_recv
// CID :Chan.try_send BEND_CID_Chan_try_send
// CID :Chan.try_recv BEND_CID_Chan_try_recv
// CID :Chan.close BEND_CID_Chan_close
// bend-ids end
// Chan
// ====

// Chan.new, send, recv, close and the try_ twins share this file.

// A parked receiver holds CHAN_RECV: the C lane parks TERM_HOLE, and a
// program can make neither. A sent value may be null (an erased proof).
const CHAN_RECV = Symbol();

function chan_done() {
  return { $: BEND_CID_Done, value: { $: BEND_CID_Unit } };
}

function chan_wait(item) {
  return { $: BEND_CID_Wait, rest: item === CHAN_RECV ? { $: BEND_CID_Unit } : item };
}

// Wakes the first waiter with x, as Ready{x} if try_ (cutting its timer).
function chan_wake(row, x) {
  const w = row.wait.shift();
  if (w.late !== undefined) {
    const ws = io_active.waits;
    ws.splice(ws.findIndex((t) => t.more === w.late), 1);
    x = { $: BEND_CID_Ready, value: x };
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
    chan_wake(row, item === CHAN_RECV ? { $: BEND_CID_None }
      : { $: BEND_CID_Fail, error: item });
  }
}

// Parks k on row with item; with ms, a timer too, whose late answers Wait:
// the first of the two to come cuts the other.
function chan_park(row, k, item, ms) {
  const w = { cont: k, item: item };
  if (ms !== undefined) {
    w.late = () => {
      row.wait.splice(row.wait.indexOf(w), 1);
      return chan_wait(item);
    };
    io_park_on(undefined, false, k, w.late, io_until(ms));
  }
  row.wait.push(w);
  return undefined;
}

// Sends without waiting: Done{}, Fail{v} if closed, undefined if it would.
function chan_put(row, value) {
  if (row.shut) {
    return { $: BEND_CID_Fail, error: value };
  }
  if (row.wait.length > 0 && row.wait[0].item === CHAN_RECV) {
    chan_wake(row, { $: BEND_CID_Some, value: value });
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
    return { $: BEND_CID_Some, value: chan_take(row) };
  }
  if (row.wait.length > 0 && row.wait[0].item !== CHAN_RECV) {
    return { $: BEND_CID_Some, value: chan_wake(row, chan_done()) };
  }
  if (row.shut) {
    return { $: BEND_CID_None };
  }
  return undefined;
}

// Ready{x} now, Wait{rest} now if ms is 0, else parks until a wake or ms.
function chan_try(row, x, item, ms, k) {
  if (x !== undefined) {
    return { $: BEND_CID_Ready, value: x };
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
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Chan_new, chan_new);
io_eff(BEND_CID_Chan_send, chan_send);
io_eff(BEND_CID_Chan_recv, chan_recv);
io_eff(BEND_CID_Chan_try_send, chan_try_send);
io_eff(BEND_CID_Chan_try_recv, chan_try_recv);
io_eff(BEND_CID_Chan_close, chan_close);
