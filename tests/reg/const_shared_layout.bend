# regression witness: native constructor cache keys expanded shared sum
# layouts into a JSON tree, using exponential memory for this small value.
import Base

type Choice<-A: Data> is Data:
  ChooseLeft{value: A}
  ChooseRight{value: A}

def main() -> Choice<Choice<Choice<Choice<Choice<Choice<
  Choice<Choice<Choice<Choice<Choice<Choice<
  Choice<Choice<Choice<Choice<Choice<Choice<
  Choice<Choice<Choice<Choice<Choice<Choice<U32>>>>>>>>>>>>>>>>>>>>>>>>:
  ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{
    ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{
      ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{
        ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{42
        }}}}}}
      }}}}}}
    }}}}}}
  }}}}}}

#|ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{ChooseLeft{42}}}}}}}}}}}}}}}}}}}}}}}}
