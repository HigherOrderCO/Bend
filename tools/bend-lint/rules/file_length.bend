# style/file-length: warn when a source file exceeds maxLines (default 500).
# Formatting cannot shorten a file's logical contents; no fix is offered.
import Base
import ../src/bend/lint.bend as Lint
import ./shared.bend as Shared

def id() -> String:
  "style/file-length"

def facts() -> Lint.Want:
  Lint.NoFacts{}

def finding(long: Bool, path: String, max: U32, count: U32) -> List<&2, Lint.Diag>:
  match long:
    case True{}:
      [
        Lint.Diag{
          Lint.Warning{},
          "Keep the file to " ++ U32.show(max) ++ " lines; it has " ++ U32.show(count) ++ ".",
          Some{Lint.Span{path, 0, 0}},
          []
        }
      ]
    case False{}:
      []

def check_source(+max: U32, path: String, +count: U32) -> List<&2, Lint.Diag>:
  finding(U32.is_gt(count, max), path, max, count)

def check(+max: U32, source: Maybe<&2, Lint.Source>) -> List<&2, Lint.Diag>:
  match source:
    case None{}:
      []
    case Some{Lint.Source{path, text, root}}:
      check_source(max, path, Shared.line_count(text))

def run(input: Lint.Input) -> IO(List<&2, Lint.Diag>):
  match input:
    case Lint.Input{sources, options}:
      IO.pure(
        List<&2, Lint.Diag>,
        check(Lint.option_number(options, "maxLines", 500), Shared.root(sources))
      )

def main() -> IO(Unit):
  Lint.serve(run)
