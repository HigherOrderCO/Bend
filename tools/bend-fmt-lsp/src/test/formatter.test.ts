import assert from "node:assert/strict";
import test from "node:test";
import { formatBend } from "../formatter.js";

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
