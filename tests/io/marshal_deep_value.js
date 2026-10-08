// bend-ids 1
// CID #Nil BEND_CID_Nil
// CID #Con BEND_CID_Con
// CID :list_new BEND_CID_list_new
// CID :list_sum BEND_CID_list_sum
// bend-ids end
function list_new(k) {
  let out = {$: BEND_CID_Nil};
  for (let i = 0; i < k; i++) {
    out = {$: BEND_CID_Con, $0: (i & 0xff) >>> 0, $1: out};
  }
  return out;
}
function list_sum(xs) {
  let n = 0;
  while (xs.$ === BEND_CID_Con) {
    n = (n + xs.$0) >>> 0;
    xs = xs.$1;
  }
  return n;
}

io_eff(BEND_CID_list_new, list_new);
io_eff(BEND_CID_list_sum, list_sum);
