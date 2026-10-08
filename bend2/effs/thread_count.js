// bend-ids 1
// CID :IO.thread_count BEND_CID_IO_thread_count
// bend-ids end
// IO
// ==

// A JS program runs one thread.
function io_thread_count() {
  return 1;
}

io_eff(BEND_CID_IO_thread_count, io_thread_count);
