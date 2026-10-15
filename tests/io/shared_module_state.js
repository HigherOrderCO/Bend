let count = 0;
function tick_bump() {
  count = count + 1;
  return count >>> 0;
}
function tick_peek() {
  return count >>> 0;
}

io_eff(CID(Tick.bump), tick_bump);
io_eff(CID(Tick.peek), tick_peek);
