# The contract between bend-lint and a rule written in Bend. A rule file
# imports this file and defines:
#
#   def id() -> String:          # namespace/name, the code of its findings
#   def facts() -> Lint.Want:    # Lint.NoFacts{}, or the facts it needs
#   def run(input: Lint.Input) -> IO(List<&2, Lint.Diag>):
#   def main() -> IO(Unit):
#     Lint.serve(run)
#
# A rule with facts reads them one at a time, with fold_facts or
# next_fact. Offsets count characters (code points) from the start of a
# file. The rule's options come from bend-lint.json; read them with
# option_number, option_flag and option_text, each with a default.
import Base

# A checked term, a type, and a node of a checked body: indexes into
# bend-lint's tables for one run. Only the effects below read them.
type Fact is Data:
  Fact{id: U32}

type Term is Data:
  Term{id: U32}

type Node is Data:
  Node{id: U32}

type Source is Data:
  Source{path: String, text: String, root: Bool}

# An option's value: a whole number (a U32), a boolean, or text.
type Value is Data:
  Num{value: U32}
  Flag{value: Bool}
  Text{value: String}

type Option is Data:
  Option{key: String, value: Value}

type Input is Data:
  Input{sources: List<&2, Source>, options: List<&2, Option>}

# File: facts of the linted file. Program: of its imports too, never Base.
type Scope is Data:
  File{}
  Program{}

# The facts a rule needs. A fact must match each list; an empty list
# matches all. kinds: the term's kind, annotations stripped (Var, Ref, App,
# ...). defs: the def whose body holds the term (a template's instances
# count as the template). names: the name a Var or Ref points to.
type Want is Data:
  NoFacts{}
  Want{scope: Scope, kinds: List<&2, String>, defs: List<&2, String>, names: List<&2, String>}

type Span is Data:
  Span{path: String, beg: U32, end: U32}

type Severity is Data:
  Error{}
  Warning{}
  Information{}
  Hint{}

# Safe keeps behavior; Suggested may change it; Dangerous may break code.
type Applicability is Data:
  Safe{}
  Suggested{}
  Dangerous{}

type Edit is Data:
  Edit{span: Span, text: String}

type Fix is Data:
  Fix{title: String, applicability: Applicability, edits: List<&2, Edit>}

type Diag is Data:
  Diag{severity: Severity, message: String, span: Maybe<&2, Span>, fixes: List<&2, Fix>}

# How many times a term may be used.
type Quantity is Data:
  Erased{}
  Once{}
  Many{}

type Use is Data:
  Use{name: String, quantity: Quantity}

# A fact as bend-lint sees it: the def it is in, whether that def is a
# template instance, the kind of its term once an annotation is stripped
# (Var, App, ...), the name a Var or Ref points to (else ""), how many
# times the term is demanded, and the spans of the checked term and of the
# stripped one.
type View is Data:
  View{owner: String, inst: Bool, kind: String, name: String, quantity: Quantity, span: Maybe<&2, Span>, inner: Maybe<&2, Span>}

# A node: its kind, annotations kept (Ann, Var, App, ...), the name a Var
# or Ref points to (else ""), its span, and its children, in bend's order.
type Shape is Data:
  Shape{kind: String, name: String, span: Maybe<&2, Span>, children: List<&2, Node>}

def input() -> IO(Input):
  import "./effects.js"

def report(diags: List<&2, Diag>) -> IO(Unit):
  import "./effects.js"

# The next fact the rule asked for, or None{} when none are left.
def next_fact() -> IO(Maybe<&2, Fact>):
  import "./effects.js"

# One step of fold_facts: stop at None{}, else run step and go on.
def fold_next(~A: Type, ~step: A -> @+f: Fact -> IO(A), m: Maybe<&2, Fact>, acc: A, again: A -> IO(A)) -> IO(A):
  match m:
    case None{}:
      IO.pure(A, acc)
    case Some{f}:
      IO.bind(A, A, step(acc, f), again)

# Runs step on each fact the rule asked for, in order, from acc; step is
# a def, given as ~name, that takes its fact as +f. Facts come one at a
# time, so a big file does not build one big list.
@unsafe
def fold_facts(~A: Type, ~step: A -> @+f: Fact -> IO(A), acc: A) -> IO(A):
  do IO<A>:
    m : Maybe<&2, Fact> <- next_fact()
    fold_next(~A, ~step, m, acc, a => fold_facts(~A, ~step, a))

def view(fact: Fact) -> IO(View):
  import "./effects.js"

# The type the fact's term was checked against, or inferred.
def type_of(fact: Fact) -> IO(Term):
  import "./effects.js"

# The type of the binder a Var fact refers to.
def binder(fact: Fact) -> IO(Maybe<&2, Term>):
  import "./effects.js"

# Whether two types are equal, as the checker compares them in the fact's
# scope.
def same(fact: Fact, a: Term, b: Term) -> IO(Bool):
  import "./effects.js"

def show(fact: Fact, t: Term) -> IO(String):
  import "./effects.js"

# The normal form of a type, in the fact's scope.
def normal(fact: Fact, t: Term) -> IO(Term):
  import "./effects.js"

# The variables the fact's term uses, each with how many times.
def uses(fact: Fact) -> IO(List<&2, Use>):
  import "./effects.js"

# The text under a span, in the file on disk.
def text(span: Span) -> IO(String):
  import "./effects.js"

# A def's checked body, if it has one.
def body(name: String) -> IO(Maybe<&2, Node>):
  import "./effects.js"

# The node a fact is about.
def node(fact: Fact) -> IO(Node):
  import "./effects.js"

def shape(n: Node) -> IO(Shape):
  import "./effects.js"

# The node's fact, if the rule asked for it.
def fact(n: Node) -> IO(Maybe<&2, Fact>):
  import "./effects.js"

def pick(hit: Bool, v: Value, later: Maybe<&2, Value>) -> Maybe<&2, Value>:
  match hit:
    case True{}:
      Some{v}
    case False{}:
      later

# The value of an option, if it is given.
def find(opts: List<&2, Option>, +key: String) -> Maybe<&2, Value>:
  match opts:
    case Nil{}:
      None{}
    case Con{Option{k, v}, rest}:
      pick(String.eq(k, key), v, find(rest, key))

def option_number_or(m: Maybe<&2, Value>, default: U32) -> U32:
  match m:
    case Some{Num{n}}:
      n
    case Some{Flag{b}}:
      default
    case Some{Text{t}}:
      default
    case None{}:
      default

# An option's value, or `default` when it is not given or of another type.
def option_number(opts: List<&2, Option>, key: String, default: U32) -> U32:
  option_number_or(find(opts, key), default)

def option_flag_or(m: Maybe<&2, Value>, default: Bool) -> Bool:
  match m:
    case Some{Flag{b}}:
      b
    case Some{Num{n}}:
      default
    case Some{Text{t}}:
      default
    case None{}:
      default

def option_flag(opts: List<&2, Option>, key: String, default: Bool) -> Bool:
  option_flag_or(find(opts, key), default)

def option_text_or(m: Maybe<&2, Value>, default: String) -> String:
  match m:
    case Some{Text{t}}:
      t
    case Some{Num{n}}:
      default
    case Some{Flag{b}}:
      default
    case None{}:
      default

def option_text(opts: List<&2, Option>, key: String, default: String) -> String:
  option_text_or(find(opts, key), default)

def serve(run: Input -> IO(List<&2, Diag>)) -> IO(Unit):
  IO.bind(Input, Unit, input(), i => IO.bind(List<&2, Diag>, Unit, run(i), report))
