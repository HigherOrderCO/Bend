def main() -> IO(String):
  with IO:
    fd <- IO/FS/open("tests/golden_tests/io/eof.txt", "r")
    match fd:
      case Result/Err:
        return wrap(append("Err: ", u24/to_string(fd.val)))
      case Result/Ok:
        bytes <- IO/FS/read_line(fd.val)
        match bytes:
          case Result/Err:
            return wrap(append("Err: ", u24/to_string(bytes.val)))
          case Result/Ok:
            txt = String/decode_utf8(bytes.val)
            return wrap(txt)

append (String/Nil) str = str
append (String/Cons x xs) str = (String/Cons x (append xs str))
