// bend-ids 1
// FID :helper BEND_FID_helper
// CID :helper BEND_CID_helper
// CID :answer BEND_CID_answer
// bend-ids end
function $CID(n) { return n + 1; }
function πCID(n) { return n + 1; }
function joined‍CID(n) { return n + 1; }
class Counter {
  #CID(n) { return n + 1; }
  value() { return this.#CID(10); }
}
function answer() {
  if (BEND_FID_helper !== BEND_CID_helper) throw new Error("wrong foreign function name");
  return $CID(9) + πCID(10) + joined‍CID(9) + new Counter().value();
}
io_eff(BEND_CID_answer, answer);
