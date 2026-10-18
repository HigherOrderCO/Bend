function base_helpers_export() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-base-export-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

def power(a: Nat, n: Nat) -> Nat:
  Nat.pow(a, n)

`);
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], {
        encoding: "utf8",
        env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
      });
    if (built.status !== 0) throw new Error(built.stderr || built.stdout);
    const lib = require(output).default;
    if (lib.power(0, 0) !== 1 || lib.power(0, 4294967296) !== 0
        || lib.power(1, 281474976710655) !== 1
        || lib.power(281474976710655, 1) !== 281474976710655) return 1;
    try {
      lib.power(2, 48);
      return 2;
    } catch {
      return 0;
    }
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Nat.base_helpers_export), base_helpers_export);
