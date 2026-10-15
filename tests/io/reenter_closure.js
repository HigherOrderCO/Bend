function wrap(g) {
  return (x) => (g(x) + 100) >>> 0;
}

io_eff(CID(wrap), wrap);
