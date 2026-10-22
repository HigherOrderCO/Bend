function echo(c) {
  return c;
}
// The host's view of c: its text, then read back as the host's own.
function seen(c) {
  return str_in(str_out(c));
}

io_eff(CID(echo), echo);
io_eff(CID(seen), seen);
