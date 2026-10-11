# regression witness: native specialization keys expanded an erased shared
# sum layout into a JSON tree, using exponential memory for this small call.
import Base

type Choice<-A: Data> is Data:
  ChooseLeft{value: A}
  ChooseRight{value: A}

def id(-A: Data, x: U32) -> U32:
  x

def main() -> U32:
  id(Choice<Choice<Choice<Choice<Choice<Choice<
    Choice<Choice<Choice<Choice<Choice<Choice<
    Choice<Choice<Choice<Choice<Choice<Choice<
    Choice<Choice<Choice<Choice<Choice<Choice<U32>>>>>>>>>>>>>>>>>>>>>>>>, 7)

#|7
