// bend-ids 1
// CID :AudioTest.count BEND_CID_AudioTest_count
// CID :AudioTest.start BEND_CID_AudioTest_start
// CID #Unit BEND_CID_Unit
// CID :AudioTest.wait BEND_CID_AudioTest_wait
// CID :AudioTest.closed BEND_CID_AudioTest_closed
// bend-ids end
// The cases run only in the Linux C lane.
io_eff(BEND_CID_AudioTest_count, () => 0);
io_eff(BEND_CID_AudioTest_start, () => ({ $: BEND_CID_Unit }));
io_eff(BEND_CID_AudioTest_wait, () => ({ $: BEND_CID_Unit }));
io_eff(BEND_CID_AudioTest_closed, () => ({ $: BEND_CID_Unit }));
