// bend-ids 1
// CID #Con BEND_CID_Con
// CID :Audio.write BEND_CID_Audio_write
// bend-ids end
// Audio
// =====

function audio_write(audio, samples) {
  let n = 0;
  for (let xs = samples; xs.$ === BEND_CID_Con; xs = xs.tail) {
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

io_eff(BEND_CID_Audio_write, audio_write);
