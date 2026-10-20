const audio_clock = Object.getOwnPropertyDescriptor(globalThis, "performance");

function audio_frame_clock(t, reset) {
  if (reset) {
    Object.defineProperty(globalThis, "performance", audio_clock);
  } else {
    // At 8000 Hz, each tick is one tenth of a stereo frame.
    Object.defineProperty(globalThis, "performance", { configurable: true,
      value: { now: () => t / 80 } });
  }
  return { $: CID(Unit) };
}

io_eff(CID(AudioTest.clock), audio_frame_clock);
