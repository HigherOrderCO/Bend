# One byte beyond the demo's response limit fails instead of truncating.
import Base
import ./http_fetch_eof.bend as HTTP

def main() -> IO(String):
  HTTP.exchange(HTTP.limit_parts([33]))

#|HTTP response exceeds 65536 bytes
#|exit 1
