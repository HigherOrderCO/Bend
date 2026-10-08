// bend-ids 1
// CID :Audio.open BEND_CID_Audio_open
// bend-ids end
// Audio
// =====

// A silent device: the ring's queue drains by the clock.
function audio_open(rate) {
  if (rate < 8000 || rate > 192000) {
    return io_fail(22);
  }
  return io_done({ rate, queued: 0, at: performance.now() });
}

io_eff(BEND_CID_Audio_open, audio_open);
