# Process.run feeds stdin without a shell, captures both streams and the
# exit code, and bounds execution and combined output on the C and JS lanes.
import Base

def answer(r: Result<&1, &1, U32 & String, U32 & (String & String)>,
  want: U32, out: String, err: String) -> String:
  match r:
    case Done{(code, (stdout, stderr))}:
      Bool.show(Bool.and(U32.is_eq(code, want),
        Bool.and(String.eq(stdout, out), String.eq(stderr, err))))
    case Fail{error}:
      "unexpected failure"

def failure(r: Result<&1, &1, U32 & String, U32 & (String & String)>,
  want: U32) -> String:
  match r:
    case Done{value}:
      "unexpected success"
    case Fail{(code, message)}:
      Bool.show(U32.is_eq(code, want))

def timeout(r: Result<&1, &1, U32 & String, U32 & (String & String)>) ->
  String:
  match r:
    case Done{value}:
      "unexpected success"
    case Fail{(+code, message)}:
      Bool.show(Bool.or(U32.is_eq(code, 110), U32.is_eq(code, 60)))

def main() -> IO(Unit):
  do IO<Unit>:
    a : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/cat", [], "Привет 🌍\n", 128, 1000)
    x : Unit <- IO.print("stdin: " ++ answer(a, 0, "Привет 🌍\n", ""))
    b : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/echo", ["x; echo injected"], "", 128, 1000)
    y : Unit <- IO.print("literal argv: " ++ answer(b, 0,
      "x; echo injected\n", ""))
    c : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "printf out; printf err >&2; exit 7"],
        "", 128, 1000)
    z : Unit <- IO.print("exit and stderr: " ++ answer(c, 7, "out", "err"))
    d : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "printf 1234"], "", 4, 1000)
    s : Unit <- IO.print("at limit: " ++ answer(d, 0, "1234", ""))
    e : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "printf 12345"], "", 4, 1000)
    t : Unit <- IO.print("over limit: " ++ failure(e, 27))
    p : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "printf abc; printf def >&2"],
        "", 5, 1000)
    q : Unit <- IO.print("combined limit: " ++ failure(p, 27))
    f : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sleep", ["2"], "", 128, 30)
    u : Unit <- IO.print("timeout: " ++ timeout(f))
    g : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/__bend_missing_executable__", [], "", 128, 1000)
    v : Unit <- IO.print("missing: " ++ failure(g, 2))
    h : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/cat", [], "", 0, 1000)
    w : Unit <- IO.print("invalid limit: " ++ failure(h, 22))
    h0 : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/cat", [], "", 1, 0)
    w0 : Unit <- IO.print("invalid timeout: " ++ failure(h0, 22))
    i : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/echo", ["bad\u{0}arg"], "", 128, 1000)
    j : Unit <- IO.print("nul argument: " ++ failure(i, 22))
    k : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/cat", [], "", 1, 1000)
    l : Unit <- IO.print("empty stdin: " ++ answer(k, 0, "", ""))
    m : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "kill -TERM $$"], "", 128, 1000)
    IO.print("signal: " ++ answer(m, 143, "", ""))
    n : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/usr/bin/perl", ["-e", "print $SIG{PIPE} // q(default)"],
        "", 128, 2000)
    o : Unit <- IO.print("sigpipe default: " ++ answer(n, 0, "default", ""))
    p : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "sleep 3 & echo hi"], "", 128, 300)
    IO.print("inherited pipe: " ++ timeout(p))
    q : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "printf A; (sleep 0.2; printf B) & exit 0"],
        "", 128, 2000)
    IO.print("descendant output: " ++ answer(q, 0, "AB", ""))
    r : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "yes & echo hi"], "", 4096, 2000)
    IO.print("runaway descendant: " ++ failure(r, 27))
    r0 : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/sh", ["-c", "exec >&- 2>&-; sleep 3"], "", 128, 300)
    IO.print("closed stdio: " ++ timeout(r0))
    r1 : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/echo", ["hi"], "", 128, 4294967295)
    IO.print("largest timeout: " ++ answer(r1, 0, "hi\n", ""))

#|stdin: True
#|literal argv: True
#|exit and stderr: True
#|at limit: True
#|over limit: True
#|combined limit: True
#|timeout: True
#|missing: True
#|invalid limit: True
#|invalid timeout: True
#|nul argument: True
#|empty stdin: True
#|signal: True
#|sigpipe default: True
#|inherited pipe: True
#|descendant output: True
#|runaway descendant: True
#|closed stdio: True
#|largest timeout: True
