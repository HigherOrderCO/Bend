// IO
// ==

// A JS program runs one thread.
function io_thread_count() {
  return 1;
}

io_eff(CID(IO.thread_count), io_thread_count);
