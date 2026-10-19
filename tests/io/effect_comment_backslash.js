// A backslash does not continue a JS comment, so the next line stays live.
function answer() {
  return 42;
}
// A path ending in a backslash, C:\, must not swallow the CID below: \
io_eff(CID(answer), answer);
