# regression witness: shared sum layouts must not expand into a tree to print main.
import Base

type Choice<-A: Data> is Data:
  ChooseLeft{value: A}
  ChooseRight{value: A}

type Answer is Data:
  Final{value: U32}
  Again{value: Answer}
  Other{value: Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<Choice<U32>>>>>>>>>>>>>>>>>>>>>>>>}

def main() -> Answer:
  Final{42}

#|Final{42}
