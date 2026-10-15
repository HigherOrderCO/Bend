function list_new(k) {
  let out = {$: CID(Nil)};
  for (let i = 0; i < k; i++) {
    out = {$: CID(Con), $0: (i & 0xff) >>> 0, $1: out};
  }
  return out;
}
function list_sum(xs) {
  let n = 0;
  while (xs.$ === CID(Con)) {
    n = (n + xs.$0) >>> 0;
    xs = xs.$1;
  }
  return n;
}

io_eff(CID(list_new), list_new);
io_eff(CID(list_sum), list_sum);
