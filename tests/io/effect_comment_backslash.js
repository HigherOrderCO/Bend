// bend-ids 1
// CID :answer BEND_CID_answer
// bend-ids end
// A backslash does not continue a JS comment, so the next line stays live.
function answer() {
  return 42;
}
// A path ending in a backslash, C:\, must not swallow the CID below: \
io_eff(BEND_CID_answer, answer);
