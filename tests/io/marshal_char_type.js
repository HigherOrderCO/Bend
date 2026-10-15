function getc() {
  return "A";
}
function pick() {
  return {$: CID(Some), value: "Z"};
}
function stamp() {
  return {$: CID(Tuple), fst: "q", snd: "ok"};
}

io_eff(CID(getc), getc);
io_eff(CID(pick), pick);
io_eff(CID(stamp), stamp);
