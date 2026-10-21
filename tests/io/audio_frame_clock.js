// bend-ids 1
// CID #Unit BEND_CID_Unit
// CID :AudioTest.clock BEND_CID_AudioTest_clock
// bend-ids end
const audio_clock = Object.getOwnPropertyDescriptor(globalThis, "performance");

function audio_frame_clock(t, reset) {
  if (reset) {
    Object.defineProperty(globalThis, "performance", audio_clock);
  } else {
    // At 8000 Hz, each tick is one tenth of a stereo frame.
    Object.defineProperty(globalThis, "performance", { configurable: true,
      value: { now: () => t / 80 } });
  }
  return { $: BEND_CID_Unit };
}

io_eff(BEND_CID_AudioTest_clock, audio_frame_clock);
