// Audio
// =====

const AUDIO_RING = 4096; // Frames; matches IO_RING in audio.c.

function audio_write(audio, samples) {
  let n = 0;
  for (let xs = samples; xs.$ === CID(Con); xs = xs.tail) {
    n += 1;
  }
  const frames = Math.floor(n / 2);
  const now = performance.now();
  const drained = Math.floor((now - audio.at) * audio.rate / 1000);
  audio.queued = Math.max(0, audio.queued - drained);
  // Carry subframe time while playing; idle time cannot drain future writes.
  audio.at = audio.queued === 0 || audio.rate === 0
    ? now : audio.at + drained * 1000 / audio.rate;
  // Complete stereo frames only; oversized writes are dropped whole.
  if (n <= AUDIO_RING * 2 && audio.queued + frames <= AUDIO_RING) {
    audio.queued += frames;
  }
  return io_tup(audio, audio.queued);
}

io_eff(CID(Audio.write), audio_write);
