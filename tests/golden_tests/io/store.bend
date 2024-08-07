(Main) =
  use path = "tests/golden_tests/io/store.txt"
  with IO {
    ask res = (IO/FS/write_file path (String/encode_utf8 "(Main) = 0"))
    res
  }
