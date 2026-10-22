# a stuck float goal displays its build: two different literals do not
# meet, and the error spells both sides as their binary32 words
import Base

law prf:
  {1.5 == 2.5 : F32}

def prf():
  {==}

law main:
  Type

def main():
  {1.5 == 2.5 : F32}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Location: prf
#| 8 | def prf():
#| 9>|   {==}
#|   |   ^^^^
#|10 |
#|- left     : 1.5  =>  1.5
#|- right    : 2.5  =>  2.5
#|exit 1
