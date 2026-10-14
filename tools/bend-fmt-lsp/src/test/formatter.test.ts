import assert from "node:assert/strict";
import test from "node:test";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { formatBend } from "../formatter.js";

const inlineLayouts = [
  [
    "import Base",
    "def f(n: Bool) -> U32:",
    "  match n: case True{}: 0",
    "           case False{}: 1",
    "",
  ].join("\n"),
  [
    "import Base",
    "def main() -> IO(Unit):",
    "  do IO<Unit>: IO.print(\"a\")",
    "               IO.print(\"b\")",
    "",
  ].join("\n"),
  [
    "import Base",
    "def main() -> IO(Unit):",
    "  do",
    "    IO<Unit>: IO.print(\"a\")",
    "              IO.print(\"b\")",
    "",
  ].join("\n"),
  [
    "import Base",
    "def main() -> IO(Unit):",
    "  do IO<",
    "    Unit>: IO.print(\"a\")",
    "           IO.print(\"b\")",
    "",
  ].join("\n"),
  [
    "import Base",
    "def main() -> IO(Unit):",
    "  do IO<Unit>",
    "    : IO.print(\"a\")",
    "      IO.print(\"b\")",
    "",
  ].join("\n"),
  [
    "import Base",
    "def f() -> IO(@x: Unit -> Unit):",
    "  do IO<@x:",
    "    Unit -> Unit>: IO.print(\"a\")",
    "                   return x => x",
    "",
  ].join("\n"),
];

test("leaves column-sensitive inline blocks unchanged", () => {
  const sources = [
    ...inlineLayouts,
    "def f(n: Bool) -> U32: match n: case True{}: 0\n                              case False{}: 1\n",
    "def main() -> IO(Unit):\n  do IO<Unit>: match flag:\n                 case True{}: IO.print(\"a\")\n                 case False{}: IO.print(\"b\")\n",
    "def main() -> IO(Unit):\n  do IO<Unit>: value: U32 <- get()\n               return value\n",
    "import Base\ndef main() -> IO(Unit):\n  do IO<(Unit : Type)>:\n    IO.print(\"a\")\n    IO.print(\"b\")\n",
  ];
  for (const source of sources) {
    for (const text of [source, source.replace(/\n/g, "\r\n").trimEnd()]) {
      for (const options of [{}, { tabSize: 4 }, { tabSize: 4, insertSpaces: false }]) {
        assert.equal(formatBend(text, options), text);
      }
    }
  }
});

test("inline layout guards ignore comments and literal contents", () => {
  const source = "# do IO<Unit>: action; match x: case y\ndef text()->String:\n    \"do IO<Unit>: action; match x: case y\"";
  assert.equal(formatBend(source), "# do IO<Unit>: action; match x: case y\ndef text() -> String:\n  \"do IO<Unit>: action; match x: case y\"");
});

test("continues formatting do blocks whose first statement is on a new line", () => {
  for (const header of ["IO<Unit>", "IO<U32>", "Effect.IO<Base.Unit>"]) {
    const source = `def main()->IO(Unit):\n    do ${header}: # first statement follows\n        IO.print("a")\n        IO.print("b")`;
    assert.equal(formatBend(source), `def main() -> IO(Unit):\n  do ${header}:  # first statement follows\n    IO.print("a")\n    IO.print("b")`);
  }
});

const bun = process.env.BEND_FMT_TEST_BUN;
const compiler = process.env.BEND_FMT_TEST_COMPILER;
test("inline blocks still check with the real Bend compiler", {
  skip: !bun || !compiler ? "set BEND_FMT_TEST_BUN and BEND_FMT_TEST_COMPILER to absolute executable/source paths" : false,
}, () => {
  const directory = mkdtempSync(join(tmpdir(), "bend-fmt-inline-"));
  try {
    for (const [index, source] of inlineLayouts.entries()) {
      const surrounded = source.replace("import Base\n", "import Base\ndef before()->U32:\n    0\n")
        + "\ndef after()->U32:\n    1\n";
      for (const [variant, text] of [source, formatBend(source), surrounded, formatBend(surrounded)].entries()) {
        const file = join(directory, `${index}-${variant}.bend`);
        writeFileSync(file, text);
        const result = spawnSync(bun!, [compiler!, file, "--check-only"], {
          cwd: directory, encoding: "utf8", timeout: 30_000,
        });
        assert.equal(result.error, undefined);
        assert.equal(result.status, 0, result.stdout + result.stderr);
      }
    }
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});

test("preserves unsupported declarations while formatting their neighbours", () => {
  for (const layout of inlineLayouts) {
    for (const eol of ["\n", "\r\n"]) {
      const source = "def before()->U32:\n    0\n" + layout + "\ndef after()->U32:\n    1\n";
      const expected = "def before() -> U32:\n  0\n" + layout + "\ndef after() -> U32:\n  1\n";
      const formatted = formatBend(source.replace(/\n/g, eol));
      assert.equal(formatted, expected.replace(/\n/g, eol));
      assert.equal(formatBend(formatted), formatted);
    }
  }
});

test("keeps attributes and uncertain boundaries with unsupported layouts", () => {
  const attributed = "@unsafe\ndef f() -> IO<Unit>:\n  do IO<Unit>: return Unit{}\n";
  assert.equal(formatBend(attributed + "def g()->U32:\n    0"),
    attributed + "def g() -> U32:\n  0");
  const uncertain = "def f() -> IO<Unit>:\n  do IO<(Unit:\ndef g()->U32:\n    0";
  assert.equal(formatBend(uncertain), uncertain);
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
