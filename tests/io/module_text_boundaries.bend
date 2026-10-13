# Exported closures stay callable; raw nested text is not mistaken for a
# non-scalar packet, and those packets expose no NUL/control payload units.
import Base

def Module.text_boundaries() -> IO(String):
  import "./module_text_boundaries.js"

def main() -> IO(Unit):
  do IO<Unit>:
    text : String <- Module.text_boundaries()
    IO.print(text)

#|closure: abcd
#|curried: abcd
#|low DC00: 56320,3
#|low DC05: 56325,3
#|payload controls: false
#|length: 2
#|surrogate: �
#|raw U32 codes: true
#|bad host types: 2
