def main:
  use bytes = [72, 101, 108, 108, 111, 44, 32, 228, 184, 150, 231, 149, 140, 33]
  use s = "Hello, 世界!"
  return (String/encode_utf8(s, []), Bytes/decode_utf8(bytes, []))
