(Main) =
  use path = "tests/golden_tests/io/missing_dir/store_fail.txt"
  (HVM.store path "(Main) = 0" @result match result {
    Result/ok: result.val;
    Result/err: result.val;
  })
