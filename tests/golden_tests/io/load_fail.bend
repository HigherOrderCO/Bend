(Main) =
  use path = "tests/golden_tests/io/missing_dir/load_fail.txt"
  with IO {
    ask file = (IO/FS/read_file path)
    (String/decode_utf8 file)
  }
