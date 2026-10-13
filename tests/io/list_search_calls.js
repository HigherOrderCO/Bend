function search_calls() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const os = require("node:os");
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "bend-list-search-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

type Item is Data:
  Item{value: U32}

def hit(item: Item) -> Bool:
  match item:
    case Item{x}:
      U32.is_eq(x, 42)

def equal(a: Item, b: Item) -> Bool:
  match a b:
    case Item{x} Item{y}:
      U32.is_eq(x, y)

def any(xs: List<&2, Item>) -> Bool:
  List.any(~&2, ~Item, ~hit, xs)

def all(xs: List<&2, Item>) -> Bool:
  List.all(~&2, ~Item, ~hit, xs)

def find(xs: List<&2, Item>) -> Maybe<&2, Item>:
  List.find(~Item, ~hit, xs)

def contains(xs: List<&2, Item>) -> Bool:
  List.contains(~Item, ~equal, xs, Item{42})
`);
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], {
        encoding: "utf8", timeout: 10000,
      });
    if (built.status !== 0) {
      throw new Error("list search build: " + (built.error?.message ?? "") + built.stdout + built.stderr);
    }
    const lib = require(output).default;
    const cases = [[], [42], [1], [42, 1, 2], [1, 42, 2], [1, 2, 42],
      [1, 2, 3], [42, 42, 42]];
    for (const name of ["any", "all", "find", "contains"]) {
      for (const values of cases) {
        const calls = [];
        const items = values.map((value, i) => ({ $: "Item",
          get value() { calls.push(i); return value; } }));
        let xs = { $: "Nil" };
        for (let i = items.length - 1; i >= 0; --i) {
          xs = { $: "Con", head: items[i], tail: xs };
        }
        const at = values.findIndex((x) => name === "all" ? x !== 42 : x === 42);
        const want = values.slice(0, at < 0 ? values.length : at + 1).map((_, i) => i);
        const result = lib[name](xs);
        const ok = name === "find"
          ? at < 0 ? result.$ === "None" : result.$ === "Some" && result.value === items[at]
          : result === (name === "all" ? at < 0 : at >= 0);
        if (!ok || JSON.stringify(calls) !== JSON.stringify(want)) {
          throw new Error(name + " on [" + values + "]: calls [" + calls
            + "], expected [" + want + "]; result correct: " + ok);
        }
      }
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Search.calls), search_calls);
