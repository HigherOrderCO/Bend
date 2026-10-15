// The length of n copies of the word undefined, as the C counts it.
function word_len(n) {
  return (n * "undefined".length) >>> 0;
}

io_eff(CID(word.len), word_len);
