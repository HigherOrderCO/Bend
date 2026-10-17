// Audio
// =====

function audio_write(audio, samples) {
  let n = 0;
  for (let xs = samples; xs.$ === CID(Con); xs = xs.tail) {
    n += 1;
  }
  // Complete stereo frames only; oversized writes are dropped whole.
  const frames = n > 8192 ? 0 : Math.floor(n / 2);
  const now = performance.now();
  audio.queued = Math.max(0,
    audio.queued - (now - audio.at) * audio.rate / 1000);
  audio.at = now;
  if (audio.queued + frames <= 4096) {
    audio.queued += frames;
  }
  return io_tup(audio, Math.floor(audio.queued));
}

io_eff(CID(Audio.write), audio_write);
