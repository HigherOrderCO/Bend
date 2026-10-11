// Audio
// =====

function audio_write(audio, samples) {
  let n = 0;
  for (let xs = samples; xs.$ === CID(Con); xs = xs.tail) {
    n += 1;
  }
  const now = Date.now();
  audio.queued = Math.max(0,
    audio.queued - (now - audio.at) * audio.rate / 1000);
  audio.at = now;
  if (audio.queued + n / 2 <= 4096) {
    audio.queued += n / 2;
  }
  return io_tup(audio, Math.floor(audio.queued));
}

io_eff(CID(Audio.write), audio_write);
