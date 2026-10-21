// bend-ids 1
// CID :pause BEND_CID_pause
// bend-ids end
// Pause only this silent device, leaving the process clock untouched.
function audio_pause(audio) {
  if (!Object.hasOwn(audio, "rate")) {
    throw new Error("audio_whole_frames: missing Audio rate field");
  }
  audio.rate = 0;
  return audio;
}

io_eff(BEND_CID_pause, audio_pause);
