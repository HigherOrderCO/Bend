// Audio
// =====

// A silent device: the ring's queue drains by the clock.
function audio_open(rate) {
  if (rate < 8000 || rate > 192000) {
    return io_fail(22);
  }
  // tests/io/audio_whole_frames.js pauses its device through the rate field.
  return io_done({ rate, queued: 0, at: performance.now() });
}

io_eff(CID(Audio.open), audio_open);
