import { afterAll, expect, test } from "bun:test";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { bendRule, lint } from "../src/lint.ts";

const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-file-length-"));
afterAll(() => fs.rmSync(dir, { recursive: true, force: true }));
const rule = await bendRule(fileURLToPath(new URL("./file_length.bend", import.meta.url)));
const fixture = (text: string, name = "main.bend") => {
  const file = path.join(dir, name);
  fs.writeFileSync(file, text);
  return file;
};
const config = (maxLines: number) => ({ rules: { "style/file-length": { maxLines } } });

test("Bend file length uses a default limit of 500 and offers no fix", async () => {
  const file = fixture("import Base\n" + "# padding\n".repeat(498) + "def main() -> U32:\n  1\n");
  const result = await lint(file, [rule]);
  expect(result.ok).toBe(true);
  expect(result.diags.map((d) => [d.code, d.message, d.fixes])).toEqual([
    ["style/file-length", "Keep the file to 500 lines; it has 501.", []],
  ]);
  expect((await lint(file, [rule], { config: config(501) })).diags).toEqual([]);
});

test("physical line counting handles empty files, LF, CRLF and absent final newlines", async () => {
  for (const [text, count] of [
    ["", 0],
    ["# one", 1],
    ["# one\n", 1],
    ["# one\r\n", 1],
    ["\n", 1],
    ["\r\n", 1],
    ["# one\n\n", 2],
    ["# one\r\n\r\n", 2],
  ] as const) {
    const result = await lint(fixture(text), [rule], { config: config(count) });
    expect([result.ok, result.diags]).toEqual([true, []]);
    if (count > 0) {
      const over = await lint(fixture(text), [rule], { config: config(count - 1) });
      expect(over.diags[0].message).toBe(
        "Keep the file to " + (count - 1) + " lines; it has " + count + ".",
      );
    }
  }
});

test("only the root file counts, not its imports", async () => {
  fixture(
    "import Base\n" + "# dependency\n".repeat(600) + "def value() -> U32:\n  1\n",
    "dependency.bend",
  );
  const file = fixture(
    "import Base\nimport ./dependency.bend as D\ndef main() -> U32:\n  D.value()\n",
  );
  expect((await lint(file, [rule], { config: config(4) })).diags).toEqual([]);
});

test("the Bend rule honors disable and severity configuration", async () => {
  const file = fixture("# first\n# second\n");
  expect(
    (await lint(file, [rule], { config: { rules: { "style/file-length": "off" } } })).diags,
  ).toEqual([]);
  const result = await lint(file, [rule], {
    config: { rules: { "style/file-length": { maxLines: 1, severity: "hint" } } },
  });
  expect(result.diags.map((d) => d.severity)).toEqual(["hint"]);
});

test("the Bend line scanner handles long files without overflowing the JS stack", async () => {
  const result = await lint(fixture("# padding\n".repeat(6000)), [rule]);
  expect(result.ok).toBe(true);
  expect(result.diags[0].message).toBe("Keep the file to 500 lines; it has 6000.");
});
