// bend-ids 1
// CID :answer BEND_CID_answer
// bend-ids end
// The same quote, "CID(...) names no constructor or def", in the JS twin.
function answer(n) {
  return (n + 2) >>> 0;
}

io_eff(BEND_CID_answer, answer);
