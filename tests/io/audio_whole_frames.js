// Pause only this silent device, leaving the process clock untouched.
function audio_pause(audio) {
  if (!Object.hasOwn(audio, "rate")) {
    throw new Error("audio_whole_frames: missing Audio rate field");
  }
  audio.rate = 0;
  return audio;
}

io_eff(CID(pause), audio_pause);
