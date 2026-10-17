// Pause only this silent device, leaving the process clock untouched.
function audio_pause(audio) {
  audio.rate = 0;
  return audio;
}

io_eff(CID(pause), audio_pause);
