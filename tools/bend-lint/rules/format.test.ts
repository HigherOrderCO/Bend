import { afterAll, describe, expect, test } from "bun:test";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { lint, applyFixes, render } from "../src/lint.ts";
import type { LintRule } from "../src/lint.ts";
import { format, formatOptions, rules } from "./format.ts";
import type { FormatOptions } from "./format.ts";

const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-format-"));
afterAll(() => fs.rmSync(dir, { recursive: true, force: true }));
const opts = { tabWidth: 2, wrapAtWidth: 100, endOfLine: "lf" } as const;
const fixture = (text: string, name = "main.bend") => {
  const file = path.join(dir, name);
  fs.writeFileSync(file, text);
  return file;
};
async function fixed(text: string, options: FormatOptions = opts) {
  const result = await lint(fixture(text), rules, {
    config: { rules: { "format/layout": options } },
  });
  if (!result.ok) throw new Error(result.diags.map(render).join("\n"));
  expect(result.ok).toBe(true);
  expect(result.diags.every((d) => d.fixes.length === 1)).toBe(true);
  expect(result.diags.every((d) => d.fixes[0].applicability === "safe")).toBe(true);
  const source = result.sources.find((s) => s.root)!;
  const output = applyFixes(source, result.diags).text;
  expect(format(output, options)).toBe(output);
  const again = await lint(fixture(output), rules, {
    config: { rules: { "format/layout": options } },
  });
  expect([again.ok, again.diags]).toEqual([true, []]);
  const Bend = result.unstable.Bend;
  const bodies = [result.unstable.book, again.unstable.book].map((book) =>
    Object.fromEntries(
      book.order.flatMap((name) => {
        const tld = book.tlds[name];
        return tld.$ === "Def" && tld.e !== undefined && !tld.b
          ? [[name, Bend.term_show(tld.e)]]
          : [];
      }),
    ),
  );
  expect(bodies[1]).toEqual(bodies[0]);
  return output;
}

describe("format/layout", () => {
  test("ordinary names cannot open delimiter groups", () => {
    const source =
      "import Base\ndef constructor() -> U32: 1\ndef toString() -> U32: 2\ndef valueOf() -> U32: 3\n";
    expect(format(source, opts)).toBe(
      "import Base\n\ndef constructor() -> U32:\n  1\n\ndef toString() -> U32:\n  2\n\ndef valueOf() -> U32:\n  3\n",
    );
  });
  test("validates its own defaults, union and unknown options", () => {
    expect(formatOptions({})).toEqual(opts);
    expect(formatOptions({ wrapAtWidth: "never" })).toEqual({ ...opts, wrapAtWidth: "never" });
    for (const bad of [0, -1, 1.5, NaN, Infinity, "2", true]) {
      expect(() => formatOptions({ tabWidth: bad })).toThrow("positive integer");
      expect(() => formatOptions({ wrapAtWidth: bad })).toThrow('"never" or a positive integer');
    }
    expect(() => formatOptions({ wrapAtWidth: "always" })).toThrow();
    for (const endOfLine of ["lf", "crlf", "preserve"] as const) {
      expect(formatOptions({ endOfLine })).toEqual({ ...opts, endOfLine });
    }
    for (const endOfLine of ["auto", "LF", "", 1, true]) {
      expect(() => formatOptions({ endOfLine })).toThrow("endOfLine must be");
    }
    expect(() => formatOptions({ breakLines: true })).toThrow("no option breakLines");
  });

  test("one sweep formats spacing, indentation, gaps, CRLF and final newline", async () => {
    const output = await fixed(
      "import   Base\r\n\r\n\r\ntype N is Data:\r\n    Z{}  \r\n    S{p:N}\r\ndef id(x:N)->N:\r\n\tx  \r\ndef main()->N: id(S{Z{}})",
    );
    expect(output).toBe(
      "import Base\n\ntype N is Data:\n  Z{}\n  S{p: N}\n\ndef id(x: N) -> N:\n  x\n\ndef main() -> N:\n  id(S{Z{}})\n",
    );
  });

  test("CRLF applies to declarations, wrapping, comments and the final newline", async () => {
    const source =
      "import Base\n#header\ndef f(first: U32, second: U32) -> U32: first\ndef main() -> U32: f(123456789, 234567890)";
    const options = { ...opts, wrapAtWidth: 20 };
    const lf = await fixed(source, options);
    expect(await fixed(source, { ...options, endOfLine: "crlf" })).toBe(
      lf.replaceAll("\n", "\r\n"),
    );
  });

  test("preserve uses the first ending in mixed files and falls back to LF", async () => {
    const options = { ...opts, endOfLine: "preserve" } as const;
    const source = "import Base\ndef main() -> U32: 1\n";
    const lf = await fixed(source);
    expect(await fixed(source.replace("\n", "\r\n"), options)).toBe(lf.replaceAll("\n", "\r\n"));
    expect(await fixed(source.replace(/\n$/, "\r\n"), options)).toBe(lf);
    expect(await fixed("def main() -> Type: Type", options)).toBe("def main() -> Type:\n  Type\n");
  });

  test("line ending options preserve literal contents", () => {
    const source = 'def main() -> String:\n  "first\r\nsecond\nthird"\n';
    for (const endOfLine of ["lf", "crlf", "preserve"] as const) {
      const options = { ...opts, endOfLine };
      const newline = endOfLine === "crlf" ? "\r\n" : "\n";
      const output = format(source, options);
      expect(output).toBe(
        "def main() -> String:" + newline + '  "first\r\nsecond\nthird"' + newline,
      );
      expect(format(output, options)).toBe(output);
    }
  });

  test("wrapped lists use one item per line and relative indentation", async () => {
    const source =
      "import Base\ndef choose(first: U32, second: U32, third: U32) -> U32:\n  first\ndef main() -> U32:\n  choose(123456789, 234567890, 345678901)\n";
    const output = await fixed(source, { tabWidth: 4, wrapAtWidth: 28 });
    expect(output).toContain(
      "def choose(\n    first: U32,\n    second: U32,\n    third: U32\n) -> U32:",
    );
    expect(output).toContain(
      "    choose(\n        123456789,\n        234567890,\n        345678901\n    )",
    );
  });

  test('"never" collapses optional wrapping but retains statement boundaries', async () => {
    const source =
      "import Base\ndef f(x: U32, y: U32) -> U32:\n  x\ndef main() -> U32:\n  x = f(\n    1,\n    2\n  )\n  f(x, 3)\n";
    expect(await fixed(source, { tabWidth: 2, wrapAtWidth: "never" })).toContain(
      "  x = f(1, 2)\n  f(x, 3)",
    );
  });

  test("preserves strings, escapes, char literals and comments", async () => {
    const source = String.raw`import Base
# header,= and http://example.org
def main() -> String:
    "a,b=c # quoted \"" ++ "longer-than-width"  # trailing,=comment
`;
    const output = await fixed(source, { tabWidth: 2, wrapAtWidth: 15 });
    expect(output).toContain('"a,b=c # quoted \\\""');
    expect(output).toContain("# header,= and http://example.org");
    expect(output).toContain("# trailing,=comment");
    expect(await fixed("import Base\ndef main() -> Char:\n  '='\n")).toContain("  '='");
  });

  test("keeps inline argument comments attached when wrapping", async () => {
    const source =
      "import Base\ndef f(x: U32, y: U32) -> U32:\n  x\ndef main() -> U32:\n  f(123, # first\n    456 # second\n  )\n";
    const output = await fixed(source, { tabWidth: 2, wrapAtWidth: 20 });
    expect(output).toContain("123,  # first\n");
    expect(output).toContain("456  # second\n");
  });

  test("comment markers get a space, while literals and special markers stay intact", async () => {
    const source = String.raw`import Base #import-comment
#header
##Heading
#!directive
#|expectation
#
# already spaced
def apostrophe() -> Char:
  '\''
def main() -> String:
  "${String.fromCodePoint(0x1f600)}#inside \"#escaped\"" #tail
`;
    const output = await fixed(source.replaceAll("\n", "\r\n"));
    expect(output).toContain("import Base  # import-comment");
    expect(output).toContain(
      "# header\n##Heading\n#!directive\n#|expectation\n#\n# already spaced",
    );
    const literal = source
      .split("\n")
      .find((line) => line.includes("#inside"))!
      .split(" #tail")[0]
      .trim();
    expect(output).toContain(literal);
    expect(output).toContain(String.fromCodePoint(0x1f600));
    expect(output).toContain("  # tail");
    const multiline =
      'import Base\ndef main() -> String:\n  "first line\n#inside literal\nlast line" #outside\n';
    expect(await fixed(multiline)).toContain('#inside literal\nlast line"  # outside');
  });

  test("handles nested type arguments, successors, quantities and templates", async () => {
    await fixed(
      "import Base\ndef identity(~T: Type, x: T) -> T:\n  x\ndef main() -> List<List<U32>>:\n  identity(~List<List<U32>>, [[1, 2], [3]])\n",
      { tabWidth: 2, wrapAtWidth: 25 },
    );
    await fixed(
      "import Base\ndef successor(+x: Nat) -> Nat:\n  1n+x\ndef main() -> Nat:\n  successor(2n)\n",
    );
    await fixed(
      "import Base\ntype Box<-Value: Data> is Data:\n  Box{value: Value}\ndef main() -> Box<U32>:\n  Box{1}\n",
      { tabWidth: 4, wrapAtWidth: 18 },
    );
    await fixed(
      "import Base\ndef main() -> List<U32>:\n  List.map(~U32, ~U32, ~(x => (x + 123456789 : U32)), [1, 2, 3, 4])\n",
      { tabWidth: 4, wrapAtWidth: 30 },
    );
    await fixed(
      "import Base\ndef identity(-T: Type, x: T) -> T:\n  x\ndef main() -> U32:\n  identity(U32 {1 : U32})\n",
    );
  });

  test("preserves nested match, do and lambda body scopes", async () => {
    await fixed(
      "import Base\ndef f(x: Nat) -> Nat:\n    match x:\n        case 0n:\n            0n\n        case 1n+p:\n            p\ndef main() -> Nat:\n    f(1n)\n",
      { tabWidth: 4, wrapAtWidth: 30 },
    );
    await fixed(
      'import Base\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    IO.print("a long message")\n',
      { tabWidth: 4, wrapAtWidth: 18 },
    );
    await fixed(
      "import Base\ndef main() -> U32:\n  f: U32 -> U32 = x => (\n    y = x\n    y\n  )\n  f(1)\n",
      { tabWidth: 4, wrapAtWidth: 20 },
    );
  });

  test("law/proof and unsafe syntax are preserved", async () => {
    await fixed(
      "import Base\nlaw same:\n  for x: U32\n  {x == x : U32}\ndef same(x):\n  {==}\n@unsafe\ndef identity(x: U32) -> U32:\n  x\ndef main() -> U32:\n  identity(1)\n",
    );
  });

  test("imports and imported law fills survive the parser guard", async () => {
    fixture("import Base\nlaw same:\n  for x: U32\n  {x == x : U32}\n", "LAWS.bend");
    await fixed("import Base\nimport ./LAWS.bend as Laws\ndef Laws.same(x):\n  {==}\n", opts);
  });

  test("indented header imports allow safe formatting across comments and blank lines", async () => {
    const source = "# header\n\n\timport Base # base\n\n# body\ndef main()->U32: 7\n";
    const expected = "# header\n\n  import Base  # base\n\n# body\ndef main() -> U32:\n  7\n";
    for (const endOfLine of ["lf", "crlf"] as const) {
      const newline = endOfLine === "crlf" ? "\r\n" : "\n";
      expect(await fixed(source.replaceAll("\n", newline), { ...opts, endOfLine })).toBe(
        expected.replaceAll("\n", newline),
      );
    }
  });

  test("indented module aliases allow safe formatting", async () => {
    fixture("import Base\ndef value() -> U32:\n  7\n", "HEADER_VALUE.bend");
    expect(
      await fixed("import Base\n  import ./HEADER_VALUE.bend as D\ndef main()->U32: D.value()\n"),
    ).toBe(
      "import Base\n  import ./HEADER_VALUE.bend as D\n\ndef main() -> U32:\n  D.value()\n",
    );
  });

  test("indented aliases preserve imported law fills without body spans", async () => {
    fixture("import Base\nlaw same:\n  for x: U32\n  {x == x : U32}\n", "HEADER_LAWS.bend");
    expect(
      await fixed("import Base\n  import ./HEADER_LAWS.bend as Laws\ndef Laws.same(x):\n  {==}\n"),
    ).toBe(
      "import Base\n  import ./HEADER_LAWS.bend as Laws\n\ndef Laws.same(x):\n  {==}\n",
    );
  });

  test("column-zero foreign body imports survive the guard", async () => {
    fixture("io_eff(CID(read), (n) => n);\n", "header-foreign.js");
    const source =
      'import Base\nlaw read:\n  U32 -> IO(U32)\ndef read(n):\nimport "./header-foreign.js"\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    n : U32 <- read(7)\n    IO.print(U32.show(n))\n';
    expect(await fixed(source)).toBe(
      'import Base\n\nlaw read:\n  U32 -> IO(U32)\n\ndef read(n):\nimport "./header-foreign.js"\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    n: U32 <- read(7)\n    IO.print(U32.show(n))\n',
    );
  });

  for (const [name, comment] of [
    ["foreign#probe.js", " #outside"],
    ["foreign  as  probe.js", ""],
    [String.raw`foreign\\#probe.js`, " #outside"],
  ]) {
    test(`preserves the foreign import path ${name}`, async () => {
      fixture("io_eff(CID(read), (n) => n);\n", name);
      const literal = `"./${name}"`;
      const source =
        `import Base\nlaw read:\n  U32 -> IO(U32)\ndef read(n):\n    import   ${literal}${comment}\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    n : U32 <- read(7)\n    IO.print(U32.show(n))\n`;
      const expected =
        `import Base\n\nlaw read:\n  U32 -> IO(U32)\n\ndef read(n):\n  import ${literal}${comment ? "  # outside" : ""}\n\ndef main() -> IO(Unit):\n  do IO<Unit>:\n    n: U32 <- read(7)\n    IO.print(U32.show(n))\n`;
      for (const endOfLine of ["lf", "crlf"] as const) {
        const newline = endOfLine === "crlf" ? "\r\n" : "\n";
        expect(await fixed(source.replaceAll("\n", newline), { ...opts, endOfLine })).toBe(
          expected.replaceAll("\n", newline),
        );
      }
    });
  }

  test("normalizes module aliases without changing path segments named as", async () => {
    fs.mkdirSync(path.join(dir, "as"));
    fixture("import Base\ndef value() -> U32:\n  7\n", "as/as-module.bend");
    expect(
      await fixed("import Base\nimport   ./as/as-module.bend   as   as #outside\ndef main() -> U32: as.value()\n"),
    ).toBe(
      "import Base\nimport ./as/as-module.bend as as  # outside\n\ndef main() -> U32:\n  as.value()\n",
    );
  });

  test("the guard rejects a changed value or scope", async () => {
    const guard: LintRule = {
      id: "test/guard",
      run: (cx) => {
        expect(cx.sameDeclarations("import Base\ndef main() -> U32:\n  2\n")).toBe(false);
        expect(cx.sameDeclarations(format(cx.root.text, opts))).toBe(true);
        return [];
      },
    };
    expect((await lint(fixture("import Base\ndef main() -> U32:\n  1\n"), [guard])).ok).toBe(true);
  });

  test("the guard compares import-looking multiline literal contents", async () => {
    const source = 'import Base\ndef main() -> String:\n  "first\nimport Base\nlast"\n';
    const guard: LintRule = {
      id: "test/literal-guard",
      run: (cx) => {
        expect(cx.sameDeclarations(cx.root.text)).toBe(true);
        expect(
          cx.sameDeclarations(cx.root.text.replace("\nimport Base\nlast", "\nimport Different\nlast")),
        ).toBe(false);
        return [];
      },
    };
    expect((await lint(fixture(source), [guard])).ok).toBe(true);
  });

  test("the guard does not mask imports after a declaration", async () => {
    const guard: LintRule = {
      id: "test/late-import",
      run: (cx) => {
        expect(cx.sameDeclarations(cx.root.text)).toBe(true);
        expect(cx.sameDeclarations(cx.root.text + "import ./missing.bend as Missing\n")).toBe(false);
        return [];
      },
    };
    expect((await lint(fixture("import Base\ndef main() -> U32:\n  7\n"), [guard])).ok).toBe(true);
  });

  test("the guard stops the module header at an unsafe annotation", async () => {
    const guard: LintRule = {
      id: "test/unsafe-header",
      run: (cx) => {
        expect(cx.sameDeclarations(cx.root.text)).toBe(true);
        expect(
          cx.sameDeclarations(cx.root.text.replace("@unsafe\n", "@unsafe\nimport Base\n")),
        ).toBe(false);
        return [];
      },
    };
    expect((await lint(fixture("import Base\n@unsafe\ndef main() -> U32:\n  7\n"), [guard])).ok).toBe(true);
  });

  test("configuration passes the union through the unchanged engine", async () => {
    await fixed("import Base\ndef main() -> U32: 1\n", { tabWidth: 2, wrapAtWidth: "never" });
    await expect(
      lint(fixture("import Base\ndef main() -> U32: 1\n"), rules, {
        config: { rules: { "format/layout": { wrapAtWidth: 0 } } },
      }),
    ).rejects.toThrow("positive integer");
  });

  test("operator chains wrap at boundaries and stay stable", async () => {
    const source = 'import Base\ndef main() -> String:\n  "aaaaaaaa" ++ "bbbbbbbb" ++ "cccccccc"\n';
    const output = await fixed(source, { tabWidth: 2, wrapAtWidth: 20 });
    expect(output).toContain('  "aaaaaaaa"\n    ++ "bbbbbbbb"\n    ++ "cccccccc"');
    expect(await fixed(source, { tabWidth: 2, wrapAtWidth: "never" })).toContain(
      '  "aaaaaaaa" ++ "bbbbbbbb" ++ "cccccccc"',
    );
  });

  test("empty files, comments and indivisible tokens have a canonical ending", () => {
    expect(format("", opts)).toBe("\n");
    expect(format("\n\n# comment\n\n", opts)).toBe("# comment\n");
    const source = 'import Base\ndef main() -> String:\n  "an indivisible literal with spaces"\n';
    const output = format(source, { tabWidth: 1, wrapAtWidth: 1 });
    expect(output).toContain(' "an indivisible literal with spaces"');
    expect(format(output, { tabWidth: 1, wrapAtWidth: 1 })).toBe(output);
  });

  test("CLI --fix writes the same whole-file formatting in one sweep", () => {
    const file = fixture("import Base\ndef main()->U32: 1  \n");
    const config = fixture(
      JSON.stringify({ rules: { "format/layout": { tabWidth: 4, wrapAtWidth: "never" } } }),
      "config.json",
    );
    const cli = fileURLToPath(new URL("../src/lint.ts", import.meta.url));
    const rule = fileURLToPath(new URL("./format.ts", import.meta.url));
    const args = [cli, file, "--rules", rule, "--config", config, "--fix", "--json"];
    const result = spawnSync(process.execPath, args, { encoding: "utf8" });
    expect(result.status).toBe(0);
    expect(result.stderr).toContain("bend-lint: fixed ");
    const text = fs.readFileSync(file, "utf8");
    expect(text).toBe("import Base\n\ndef main() -> U32:\n    1\n");
    const again = spawnSync(process.execPath, args, { encoding: "utf8" });
    expect([again.status, again.stderr, JSON.parse(again.stdout)]).toEqual([
      0,
      "",
      { ok: true, findings: [] },
    ]);
    expect(fs.readFileSync(file, "utf8")).toBe(text);
  });
});
