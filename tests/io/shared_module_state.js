// bend-ids 1
// CID :Tick.bump BEND_CID_Tick_bump
// CID :Tick.peek BEND_CID_Tick_peek
// bend-ids end
let count = 0;
function tick_bump() {
  count = count + 1;
  return count >>> 0;
}
function tick_peek() {
  return count >>> 0;
}

io_eff(BEND_CID_Tick_bump, tick_bump);
io_eff(BEND_CID_Tick_peek, tick_peek);
