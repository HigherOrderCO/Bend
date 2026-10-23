import assert from "node:assert/strict";
import test from "node:test";
import { spawnSync } from "node:child_process";
import { existsSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { formatBend } from "../formatter.js";

// Bend pins the lines after a body that shares a line with its `do`, `match`,
// `case` or `def` header, `=>` lambda opener, `;`, `(` or parenthesized `,`
// to that body's column. [source, formatted]
const pinned = [
  [
    "import Base\ndef f(a:Array<U32>)->Array<U32>: b=a; b[0]<-7\n                                      b[1]<-8\n\ndef main() -> Array<U32>:\n  f([0 : U32 * 2n])\n",
    "import Base\ndef f(a: Array<U32>) -> Array<U32>: b = a; b[0] <- 7\n                                           b[1] <- 8\n\ndef main() -> Array<U32>:\n  f([0 : U32 * 2n])\n",
  ],
  ...[
    "(1 < 2 : U32) == True{} : Bool",
    "(2 > 1 : U32) == True{} : Bool",
    "(1 << 1n : U32) == 2 : U32",
    "(2 >> 1n : U32) == 1 : U32",
  ].map((goal) => {
    const head = `def update(a: Array<U32>, -ok: {${goal}}) -> Array<U32>: a[0] <- 7`;
    const source = "import Base\n\n" + head + "\n"
      + " ".repeat(head.indexOf("a[0]")) + "a[1] <- 8\n\n"
      + "def main() -> Array<U32>:\n  update([0 : U32 * 2n], {==})\n";
    return [source, source];
  }),
  [
    "import Base\n\ndef main() -> Array<U32>:\n  f : (Array<U32> -> Array<U32>) =\n    (a => a[0] <- 7\n          a[1] <- 8)\n  f([0 : U32 * 2n])\n",
    "import Base\n\ndef main() -> Array<U32>:\n  f : (Array<U32> -> Array<U32>) =\n    (a => a[0] <- 7\n          a[1] <- 8)\n  f([0 : U32 * 2n])\n",
  ],
  [
    "import Base\n\ndef apply(f: Array<U32> -> Array<U32>, a: Array<U32>) -> Array<U32>:\n  f(a)\n\ndef main() -> Array<U32>:\n  apply(a=>x : Result<&2,&2,U32,U32> = Done{1};a[0]<-7\n                                               a[1]<-8,\n                                        [0 : U32 * 2n])\n",
    "import Base\n\ndef apply(f: Array<U32> -> Array<U32>, a: Array<U32>) -> Array<U32>:\n  f(a)\n\ndef main() -> Array<U32>:\n  apply(a => x : Result<&2, &2, U32, U32> = Done{1}; a[0] <- 7\n                                                     a[1] <- 8,\n    [0 : U32 * 2n])\n",
  ],
  [
    "import Base\n\ndef main() -> Array<U32>:\n  f : (Array<U32> -> Array<U32>) = (a=>a[0]<-7;a[1]<-8\n                                               a[0]<-9)\n  f([0 : U32 * 2n])\n",
    "import Base\n\ndef main() -> Array<U32>:\n  f : (Array<U32> -> Array<U32>) = (a => a[0] <- 7; a[1] <- 8\n                                                    a[0] <- 9)\n  f([0 : U32 * 2n])\n",
  ],
  [
    "import Base\ndef main() -> Array<U32>:\n    f : Array<U32> -> Array<U32> = a => a[0] <- 7\n                                        a[1] <- 8\n    f([0 : U32 * 2n])\n",
    "import Base\ndef main() -> Array<U32>:\n  f : Array<U32> -> Array<U32> = a => a[0] <- 7\n                                      a[1] <- 8\n  f([0 : U32 * 2n])\n",
  ],
  [
    "import Base\ndef main() -> IO(Unit):\n    do IO<Unit>: IO.print(\"a\")\n                 IO.print(\"b\")\n",
    "import Base\ndef main() -> IO(Unit):\n  do IO<Unit>: IO.print(\"a\")\n               IO.print(\"b\")\n",
  ],
  [
    "import Base\ndef f(a: Array<U32>, b: Bool) -> Array<U32>:\n    match b:\n      case True{}: a[0] <- 7\n                   a[1] <- 8\n      case False{}: a\n\n"
      + "def g(a: Array<U32>) -> Array<U32>: a[0] <- 5\n                                    a[1] <- 6\n\ndef main() -> Array<U32>:\n  g(f([0 : U32 * 2n], True{}))\n",
    "import Base\ndef f(a: Array<U32>, b: Bool) -> Array<U32>:\n  match b:\n    case True{}: a[0] <- 7\n                 a[1] <- 8\n    case False{}: a\n\n"
      + "def g(a: Array<U32>) -> Array<U32>: a[0] <- 5\n                                    a[1] <- 6\n\ndef main() -> Array<U32>:\n  g(f([0 : U32 * 2n], True{}))\n",
  ],
  [
    "import Base\ndef f(n: Bool) -> U32:\n    match n: case True{}: 0\n             case False{}: 1\n\ndef main() -> U32:\n  f(False{})\n",
    "import Base\ndef f(n: Bool) -> U32:\n  match n: case True{}: 0\n           case False{}: 1\n\ndef main() -> U32:\n  f(False{})\n",
  ],
  [
    "import Base\ndef main() -> IO(Unit):\n  do IO<Unit>:\n      IO.print(String.concat([\"a\",\n    \"c\"]))\n      IO.print(\"b\")\n",
    "import Base\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    IO.print(String.concat([\"a\",\n    \"c\"]))\n    IO.print(\"b\")\n",
  ],
  [
    "import Base\ndef main() -> IO(Unit):\n  do IO<Unit>:\n\t\tIO.print(\"a\")\n  IO.print(\"b\")\n",
    "import Base\ndef main() -> IO(Unit):\n  do IO<Unit>:\n  IO.print(\"a\")\n  IO.print(\"b\")\n",
  ],
  [
    "import Base\ndef pick(b: Bool) -> IO(Unit):\n      match b:\n        case True{}: do IO<Unit>: IO.print(\"t\")\n                                  # pinned too\n                                  IO.print(String.concat([\"u\",\n                \"v\"]))\n            # a comment left of the column\n                                  IO.print(\"w\")\n        case False{}: IO.print(\"f\")\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    pick(True{})\n    pick(False{})\n",
    "import Base\ndef pick(b: Bool) -> IO(Unit):\n  match b:\n    case True{}: do IO<Unit>: IO.print(\"t\")\n                              # pinned too\n                              IO.print(String.concat([\"u\",\n      \"v\"]))\n      # a comment left of the column\n                              IO.print(\"w\")\n    case False{}: IO.print(\"f\")\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    pick(True{})\n    pick(False{})\n",
  ],
  [
    "import Base\ndef f() -> IO(@x: U32 -> U32):\n    do IO<@x: U32->U32>: IO.print(\"a\")\n                         return x => (x + 1 : U32)\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    g : (@x: U32 -> U32) <- f()\n    IO.print(U32.show(g(2)))\n",
    "import Base\ndef f() -> IO(@x: U32 -> U32):\n  do IO<@x: U32 -> U32>: IO.print(\"a\")\n                         return x => (x + 1 : U32)\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    g : (@x: U32 -> U32) <- f()\n    IO.print(U32.show(g(2)))\n",
  ],
  [
    "import Base\ndef f(a: Array<U32>, b: Bool, c: Bool) -> Array<U32>:\n    match b:\n      case True{}: match c:\n                     case True{}: a[0] <- 7\n                                  a[1] <- 8\n                     case False{}: a\n      case False{}: a\n\ndef main() -> Array<U32>:\n    f([0 : U32*2n], True{}, True{})\n",
    "import Base\ndef f(a: Array<U32>, b: Bool, c: Bool) -> Array<U32>:\n  match b:\n    case True{}: match c:\n                   case True{}: a[0] <- 7\n                                a[1] <- 8\n                   case False{}: a\n    case False{}: a\n\ndef main() -> Array<U32>:\n  f([0 : U32 * 2n], True{}, True{})\n",
  ],
  [
    "import Base\ndef main()->U32 & Array<U32>:\n  a = [0 : U32 * 2n]\n  (17,a[0]<-3\n      a[1]<-5)\n",
    "import Base\ndef main() -> U32 & Array<U32>:\n  a = [0 : U32 * 2n]\n  (17, a[0] <- 3\n       a[1] <- 5)\n",
  ],
  [
    "import Base\ndef finish(p:U32 & Array<U32>,n:U32)->Array<U32>:\n  (x,a) = p\n  a[0] <- U32.add(x,n)\ndef main()->Array<U32>:\n  a = [0 : U32 * 2n]\n  finish((17,a[0]<-3\n             a[1]<-5),\n    11)\n",
    "import Base\ndef finish(p: U32 & Array<U32>, n: U32) -> Array<U32>:\n  (x, a) = p\n  a[0] <- U32.add(x, n)\ndef main() -> Array<U32>:\n  a = [0 : U32 * 2n]\n  finish((17, a[0] <- 3\n              a[1] <- 5),\n    11)\n",
  ],
];

test("moves pinned lines as far as the body they line up with", () => {
  for (const [source, expected] of pinned) {
    assert.equal(formatBend(source), expected);
    assert.equal(formatBend(expected), expected);
  }
});

test("pins with tabs count a tab as one column, as the parser does", () => {
  const source = "def f(n: Bool) -> U32:\n    match n: case True{}: 0\n             case False{}: 1\n";
  const once = formatBend(source, { tabSize: 4, insertSpaces: false });
  assert.equal(once, "def f(n: Bool) -> U32:\n\tmatch n: case True{}: 0\n\t         case False{}: 1\n");
  assert.equal(formatBend(once, { tabSize: 4, insertSpaces: false }), once);
});

test("formats do blocks whose first statement is on the next line", () => {
  for (const header of ["IO<Unit>", "Result<&2, &2, U32, U32>", "IO<List<&2, U32>>", "IO<Client<P, S>>"]) {
    const source = `def main()->IO(Unit):\n    do ${header}: # first statement follows\n        IO.print("a")\n        IO.print("b")`;
    assert.equal(formatBend(source), `def main() -> IO(Unit):\n  do ${header}:  # first statement follows\n    IO.print("a")\n    IO.print("b")`);
  }
});

// The repo's bun and compiler, unless BEND_FMT_TEST_BUN or
// BEND_FMT_TEST_COMPILER name others.
const bun = process.env.BEND_FMT_TEST_BUN ?? "bun";
const compiler = process.env.BEND_FMT_TEST_COMPILER ?? fileURLToPath(new URL("../../../../bend2/main.ts", import.meta.url));
const runnable = spawnSync(bun, ["--version"]).status === 0 && existsSync(compiler);
test("pinned layouts run the same after formatting", { skip: !runnable && "needs bun and bend2/main.ts" }, () => {
  const directory = mkdtempSync(join(tmpdir(), "bend-fmt-pinned-"));
  const run = (text: string) => {
    const file = join(directory, "main.bend");
    writeFileSync(file, text);
    const result = spawnSync(bun, [compiler, file], { encoding: "utf8", timeout: 60_000 });
    return [result.status, result.stdout + result.stderr];
  };
  try {
    for (const [source] of pinned) {
      const before = run(source);
      assert.equal(before[0], 0, String(before[1]));
      for (const options of [{}, { tabSize: 4 }, { insertSpaces: false }]) {
        assert.deepEqual(run(formatBend(source, options)), before);
      }
    }
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("formats Bend 2 declarations and nested blocks", () => {
  const source = [
    "import   Base",
    "",
    "def  area ( x : Shape )-> U32:",
    "     match   x:",
    "          case  Circle{ +r }:",
    "             ( 3*r*r : U32 )",
    "",
  ].join("\n");
  assert.equal(formatBend(source), [
    "import Base",
    "",
    "def area(x : Shape) -> U32:",
    "  match x:",
    "    case Circle{+r}:",
    "      (3 * r * r : U32)",
    "",
  ].join("\n"));
});

test("preserves comments, literals, line endings, and final newline state", () => {
  const source = "def main()->String:\r\n\t\"a # b\\n\"   # exact comment";
  assert.equal(formatBend(source), "def main() -> String:\r\n  \"a # b\\n\"  # exact comment");
});

for (const [name, source, expected] of [
  [
    "relative",
    "import   lib/dep.bend   as   Dep # dependency\n\ndef main()->U32:\n    Dep.answer()\n",
    "import lib/dep.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
  [
    "absolute",
    "import   /tmp/bend-lib/dep.bend   as   Dep # dependency\n\ndef main()->U32:\n    Dep.answer()\n",
    "import /tmp/bend-lib/dep.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
  [
    "dot-relative",
    "import   ./lib/dep.bend   as   Dep # dependency\n\ndef main()->U32:\n    Dep.answer()\n",
    "import ./lib/dep.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
  [
    "parent-relative",
    "import   ../lib/dep.bend   as   Dep# dependency\n\ndef main()->U32:\n    Dep.answer()\n",
    "import ../lib/dep.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
  [
    "content-hash",
    "import   0x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef/main.bend   as   Dep\n\ndef main()->U32:\n    Dep.answer()\n",
    "import 0x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef/main.bend as Dep\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
  [
    "named-package",
    "import   math-lib@1.2.3/main.bend   as   Dep # dependency\n\ndef main()->U32:\n    Dep.answer()\n",
    "import math-lib@1.2.3/main.bend as Dep  # dependency\n\ndef main() -> U32:\n  Dep.answer()\n",
  ],
]) {
  test(`preserves ${name} module paths while formatting neighbouring code`, () => {
    assert.equal(formatBend(source), expected);
    assert.equal(formatBend(expected), expected);
  });
}

test("keeps separated import path parts separated", () => {
  assert.equal(formatBend("import lib /dep.bend as Dep"), "import lib / dep.bend as Dep");
});

test("preserves separated annotated arguments and adjacent constructors", () => {
  for (const [source, expected] of [
    ["choose(id   {42:U32})", "choose(id {42: U32})"],
    ["choose(Fns.id {42:U32})", "choose(Fns.id {42: U32})"],
    ["use(Nat {1n==1n:Nat})", "use(Nat {1n == 1n: Nat})"],
    ["use(P {==})", "use(P {==})"],
    ["choose(id,{42:U32})", "choose(id, {42: U32})"],
    ["Wrap{Inner{1}}", "Wrap{Inner{1}}"],
    ["\\ { Case:handler }", "\\{Case: handler}"],
  ]) {
    assert.equal(formatBend(source), expected);
    assert.equal(formatBend(expected), expected);
  }
});

test("formats Base and quoted foreign imports without changing comments or arithmetic", () => {
  const source = "import   Base # base\n# import lib/dep.bend as Dep\n\ndef read()->IO(U32):\n    import   \"./lib/foreign-dep.js\" # foreign\n\ndef main()->U32:\n    (6/3-1:U32)\n";
  const expected = "import Base  # base\n# import lib/dep.bend as Dep\n\ndef read() -> IO(U32):\n  import \"./lib/foreign-dep.js\"  # foreign\n\ndef main() -> U32:\n  (6 / 3 - 1: U32)\n";
  assert.equal(formatBend(source), expected);
  assert.equal(formatBend(expected), expected);
});

test("preserves escaped quotes and hashes inside literals", () => {
  for (const literal of [String.raw`"a \" # b"`, String.raw`'a \' # b'`]) {
    assert.equal(formatBend(literal + "# exact"), literal + "  # exact");
  }
});

test("preserves angle spacing because Bend uses it to disambiguate syntax", () => {
  const source = "def f(x:U32)->U32:\n    y=x < 2\n    List<List<U32>>{}";
  assert.equal(formatBend(source), "def f(x: U32) -> U32:\n  y = x < 2\n  List<List<U32>>{}");
});

test("uses tabs when requested and is idempotent", () => {
  const source = "def main() -> U32:\n    match x:\n      case 0 n:\n        0";
  const once = formatBend(source, { tabSize: 4, insertSpaces: false });
  assert.equal(once, "def main() -> U32:\n\tmatch x:\n\t\tcase 0 n:\n\t\t\t0");
  assert.equal(formatBend(once, { tabSize: 4, insertSpaces: false }), once);
});

test("keeps Bend prefix forms glued without changing infix operators", () => {
  const source = "law use:\n    for + value: U32\n    List< & 2,U32>\n    & item:U32 -> { item==item:U32}\n    \\ {}\n    % proof : {==}";
  assert.equal(formatBend(source), "law use:\n  for +value: U32\n  List<&2, U32>\n  &item: U32 -> {item == item: U32}\n  \\{}\n  %proof : {==}");
});

test("keeps floating point exponents intact", () => {
  assert.equal(formatBend("def x()->F32:\n  1.25e-4"), "def x() -> F32:\n  1.25e-4");
});

test("preserves adjacency in natural-number successor patterns", () => {
  assert.equal(formatBend("case 1n+p:\n  p"), "case 1n+p:\n  p");
  assert.equal(formatBend("case 1n++p:\n  p"), "case 1n++p:\n  p");
  assert.equal(formatBend("value=1n + p"), "value = 1n + p");
});

test("keeps adjacent parallel binders and template quantities", () => {
  assert.equal(formatBend("+a +b=x y"), "+a +b = x y");
  assert.equal(formatBend("f(~ & 2,~T,[1,2])"), "f(~&2, ~T, [1, 2])");
});

test("formats dot operators and keeps continuation operators infix", () => {
  assert.equal(formatBend("x=(6 .&. 3 .|. 8 : U32)\n  + value"), "x = (6 .&. 3 .|. 8 : U32)\n  + value");
});

test("keeps parallel execution call suffixes glued", () => {
  assert.equal(formatBend("result = run ! (20n)"), "result = run!(20n)");
});

test("keeps unsafe declaration suffixes adjacent to names", () => {
  assert.equal(formatBend("def value?()->U32:\n  1"), "def value?() -> U32:\n  1");
  assert.equal(formatBend("def value? ()->U32:\n  1"), "def value?() -> U32:\n  1");
  assert.equal(formatBend("def\n  value?()->U32:\n  1"), "def\n  value?() -> U32:\n  1");
  assert.equal(formatBend("def # declaration\n  value? ()->U32:\n  1"), "def  # declaration\n  value?() -> U32:\n  1");
});

test("keeps unsafe suffixes on dotted declarations with inline attributes", () => {
  assert.equal(formatBend("@unsafe def Value.get? ()->U32:\n  1"), "@unsafe def Value.get?() -> U32:\n  1");
});

test("preserves invalid gaps before unsafe declaration suffixes", () => {
  assert.equal(formatBend("def value ?()->U32:\n  1"), "def value ?() -> U32:\n  1");
});

test("keeps prefix holes glued", () => {
  assert.equal(formatBend("def value()->U32:\n  ?TODO\n  result=?help"), "def value() -> U32:\n  ?TODO\n  result = ?help");
  assert.equal(formatBend("case?help:"), "case ?help:");
});

test("leaves unterminated literals unchanged", () => {
  for (const literal of [
    "\"", "'", "\"unfinished", "'unfinished", "\"unfinished\\", "'unfinished\\",
  ]) {
    const source = "def main() -> String:\n  " + literal;
    assert.equal(formatBend(source), source);
  }
});

test("semicolon pins respect delimiters, literals, and comments", () => {
  for (const [source, expected] of [
    ["def f() -> U32:\n    use(x=>x,\n                  3)\n", "def f() -> U32:\n  use(x => x,\n    3)\n"],
    ["def f() -> U32:\n    (x=>x\n                  )\n", "def f() -> U32:\n  (x => x\n    )\n"],
    ["def f() -> U32:\n    (x:U32=1; x\n                  )\n", "def f() -> U32:\n  (x: U32 = 1; x\n    )\n"],
    ["def f() -> List<U32>:\n    [1, (x:U32=2; x),\n                  3]\n", "def f() -> List<U32>:\n  [1, (x: U32 = 2; x),\n    3]\n"],
    ['def f() -> String:\n    "a; b" # ; comment\n', 'def f() -> String:\n  "a; b"  # ; comment\n'],
    ['def f() -> String:\n    "a;\n                  b"\n', 'def f() -> String:\n    "a;\n                  b"\n'],
  ]) {
    assert.equal(formatBend(source), expected);
    assert.equal(formatBend(expected), expected);
  }
});

test("mixed line endings and the final-newline state survive formatting", () => {
  for (const source of [
    "import Base\r\ndef main()->U32:\n    7\r\n",
    "import Base\ndef main()->U32:\r\n    7",
    "import Base\r\ndef main()->U32:\r\n    7\r\n",
  ]) {
    const formatted = formatBend(source);
    assert.deepEqual(formatted.match(/\r?\n/g), source.match(/\r?\n/g));
    assert.equal(formatted.endsWith("\n"), source.endsWith("\n"));
    assert.equal(formatBend(formatted), formatted);
  }
});

test("preserves body layout across dedents, declarations, and delimiters", { skip: !runnable && "needs bun and bend2/main.ts" }, () => {
  const cases = [
    ["import Base\ndef g(a:Bool,b:Bool)->Nat:\n  match a:\n    case True{}:\n      match b:\n        case True{}:\n  10n\n          case False{}:\n  20n\n    case False{}:\n  30n\ndef main()->Nat:\n  g(True{},False{})\n", "20n\n"],
    ["import Base\ndef g(a: Bool, b: Bool) -> Nat:\n  match a:\n    case True{}:\n      match b:case True{}:\n  10n\n              case False{}:\n  20n\n    case False{}:\n  30n\ndef main() -> Nat:\n  g(True{}, False{})\n", "20n\n"],
    ["import Base\ndef  g(a: Bool, b: Bool) -> IO(Unit):\n  match a:\n    case True{}:\n      match b: case True{}:\n  IO.print(\"10\")\n               case False{}:\n                 do IO<Unit>:\n                   IO.print(\"20\")\n                   IO.print(\"21\")\n    case False{}:\n  IO.print(\"30\")\ndef main() -> IO(Unit):\n  g(True{}, False{})\n", "20\n21\n"],
    ["import Base\ndef  g(a: Bool, b: Bool, c: Bool) -> Nat:\n  match a:\n    case True{}:\n      match b: case True{}:\n  10n\n               case False{}:\n                 match c:\n                   case True{}:\n  20n\n                   case False{}:\n  21n\n    case False{}:\n  30n\ndef main() -> Nat:\n  g(True{}, False{}, True{})\n", "20n\n"],
    ["import Base\ndef g(b:Bool)->Type:\n  match b:\n    case True{}:\n@x: U32 -> U32\n    case False{}:\nU32\ndef main() -> g(False{}):\n  7\n", "7\n"],
    ["import Base\ndef g(a:Bool,b:Bool)->Type:\n  match a:\n    case True{}:\n      match b:\n        case True{}:\n(\n@unsafe: U32 -> U32\n)\n          case False{}:\n  U32\n    case False{}:\n  U32\ndef main() -> g(True{}, False{}):\n  7\n", "7\n"],
    ["import Base\ndef old(a: Bool) -> U32:\n  match a:\n          case True{}: 1\n          case False{}: 2\n@\n  unsafe def newer(b: Bool) -> U32:\n  match b:\n    case True{}:\n10\n    case False{}:\n20\ndef main() -> U32:\n  newer(False{})\n", "20\n"],
    ["import Base\ndef old(a: Bool) -> U32:\n  match a:\n          case True{}: 1\n          case False{}: 2\n@ # decorator\nunsafe # qualifier\n  def newer(b: Bool) -> U32:\n  match b:\n    case True{}:\n10\n    case False{}:\n20\ndef main() -> U32:\n  newer(False{})\n", "20\n"],
    ["import Base\ndef main() -> Array<U32>:\n  a = [0 : U32 * 2n]\n  (a[0]<-3\n   a[1]<-5)\n", "[3, 5]\n"],
    ["import Base\ndef finish(a:Array<U32>,n:U32)->Array<U32>:\n  a[0] <- n\ndef main()->Array<U32>:\n  a = [0 : U32 * 2n]\n  finish((a[0]<-3\n          a[1]<-5\n    ),\n    11)\n", "[11, 5]\n"],
    ["import Base\ndef main() -> Array<U32>:\n  a = [0 : U32 * 2n]\n  (a[0]<-7;a[1]<-8\n           a[0]<-9)\n", "[9, 8]\n"],
    ["import Base\ndef main() -> Array<U32>:\n  a = [0 : U32 * 2n]\n  (a[0]<-7;x : Result<&2,&2,U32,U32> = Done{1};a[1]<-8\n                                               a[0]<-9)\n", "[9, 8]\n"],
    ["import Base\ndef keep(a: Array<U32>, n: U32) -> Array<U32>:\n  a\ndef main() -> Array<U32>:\n  a = [0 : U32 * 2n]\n  keep((a[0]<-7;a[1]<-8\n                a[0]<-9\n    ),\n    0)\n", "[9, 8]\n"],
  ];
  const directory = mkdtempSync(join(tmpdir(), "bend-fmt-cases-"));
  const file = join(directory, "main.bend");
  const run = (text: string) => {
    writeFileSync(file, text);
    const result = spawnSync(bun, [compiler, file], { encoding: "utf8", timeout: 60_000 });
    assert.equal(result.status, 0, result.stdout + result.stderr);
    return result.stdout;
  };
  try {
    for (const [source, expected] of cases) {
      assert.equal(run(source), expected);
      for (const options of [{}, { tabSize: 4 }, { insertSpaces: false }]) {
        const formatted = formatBend(source, options);
        assert.notEqual(formatted, source);
        assert.equal(run(formatted), expected);
        assert.equal(formatBend(formatted, options), formatted);
      }
    }
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
