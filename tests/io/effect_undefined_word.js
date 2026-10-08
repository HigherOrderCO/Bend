// bend-ids 1
// CID :word.len BEND_CID_word_len
// bend-ids end
// The length of n copies of the word undefined, as the C counts it.
function word_len(n) {
  return (n * "undefined".length) >>> 0;
}

io_eff(BEND_CID_word_len, word_len);
