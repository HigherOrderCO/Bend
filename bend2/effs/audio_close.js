// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :Audio.close BEND_CID_Audio_close
// bend-ids end
// Audio
// =====

function audio_close(audio) {
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_Audio_close, audio_close);
