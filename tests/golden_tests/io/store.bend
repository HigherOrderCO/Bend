(Main) : (IO (Result None u24)) =
  use path = "tests/golden_tests/io/store.txt"
  with IO {
    (IO/FS/write_file path (String/encode_utf8 "(Main) = 0"))
  }
