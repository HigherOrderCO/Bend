// Audio
// =====

// A silent device: the ring's queue drains by the clock.
function audio_open(rate) {
  if (rate < 8000 || rate > 192000) {
    return io_fail(22);
  }
  return io_done({ rate, queued: 0, at: Date.now() });
}

io_eff(CID(Audio.open), audio_open);
