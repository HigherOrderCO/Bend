// bend-ids 1
// CID :Nat.host_export BEND_CID_Nat_host_export
// bend-ids end
function nat_host_export() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-nat-export-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

def identity(n: Nat) -> Nat:
  n

def inc(n: Nat) -> Nat:
  Nat.add(n, 1n)

def add(a: Nat, b: Nat) -> Nat:
  Nat.add(a, b)
`);
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], {
        encoding: "utf8",
        env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
      });
    if (built.status !== 0) {
      return 1;
    }
    const lib = require(output).default;
    const err = "bend: expected a Nat host number: an integer from 0 to 2^53";
    const bad = [() => lib.inc(-1), () => lib.inc(1.5), () => lib.inc(1n),
      () => lib.inc(2 ** 53 + 2), () => lib.add(1)(-1),
      () => lib.identity(Infinity), () => lib.identity(NaN)];
    if (bad.some((f) => { try { f(); } catch (e) { return e !== err; }
      return true; })) {
      return 2;
    }
    return lib.inc(41) === 42 && lib.add(2)(3) === 5
      && [0, 2 ** 48, 2 ** 53].every((n) => lib.identity(n) === n) ? 0 : 3;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(BEND_CID_Nat_host_export, nat_host_export);
