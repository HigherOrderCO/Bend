# Real Node imports preserve cached modules and reload changed Bend code
# through query/fragment identities, without treating ordinary JS or data
# modules whose URL ends in .bend as Bend files.
import Base

def Module.node_urls() -> IO(U32):
  import "./node_module_urls.js"

def main() -> IO(Unit):
  do IO<Unit>:
    status : U32 <- Module.node_urls()
    IO.print(U32.show(status))

#|0
