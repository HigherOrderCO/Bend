(Main) : (IO (Result String u24)) =
  use path = "tests/golden_tests/io/missing_dir/load_fail.txt"
  with IO {
    ask file = (IO/FS/read_file path)
    match file {
      Result/Err:
        (wrap (Result/Err (file.val)))
      Result/Ok:
        (wrap (Result/Ok (String/decode_utf8 file.val)))
    }
  }
