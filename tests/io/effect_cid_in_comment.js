// The same quote, "CID(...) names no constructor or def", in the JS twin.
function answer(n) {
  return (n + 2) >>> 0;
}

io_eff(CID(answer), answer);
