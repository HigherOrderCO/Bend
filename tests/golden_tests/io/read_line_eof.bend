def main:
  with IO:
    fd <- IO/done_on_err(IO/FS/open("tests/golden_tests/io/eof.txt", "r"))
    bytes <- IO/FS/read_line(fd)
    txt = String/decode_utf8(bytes)
    return wrap(txt)
