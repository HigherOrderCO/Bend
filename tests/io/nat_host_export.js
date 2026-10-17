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
    const options = {
      encoding: "utf8",
      env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
    };
    const built = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), input, "-o", output], options);
    if (built.status !== 0) {
      return 1;
    }
    const lib = require(output).default;
    const bad = [() => lib.inc(-1), () => lib.inc(1.5), () => lib.inc(1n),
      () => lib.inc(2 ** 53 + 2), () => lib.add(1)(-1),
      () => lib.identity(Infinity), () => lib.identity(NaN)];
    if (bad.some((f) => { try { f(); } catch { return false; }
      return true; })) {
      return 2;
    }
    if (lib.inc(41) !== 42 || lib.add(2)(3) !== 5
      || [0, 2 ** 48, 2 ** 53].some((n) => lib.identity(n) !== n)) {
      return 3;
    }
    const negative = path.join(dir, "negative.bend");
    const native = path.join(dir, "negative");
    const script = path.join(dir, "negative.js");
    fs.writeFileSync(negative, `import Base

law get:
  IO(Nat)

def get():
  import "./nat_host_neg.c"
  import "./nat_host_neg.js"

def main() -> IO(Unit):
  IO.bind(Nat, Unit, get(), n => IO.print(Nat.show(n)))
`);
    for (const ext of ["c", "js"]) {
      const file = "nat_host_neg." + ext;
      fs.copyFileSync(path.resolve("tests/io", file), path.join(dir, file));
    }
    const compiled = child.spawnSync(process.execPath,
      [path.resolve("bend2/main.ts"), negative, "-o", native, "-o", script], options);
    if (compiled.status !== 0) {
      return 4;
    }
    return child.spawnSync(native, [], options).status === 1
      && child.spawnSync(process.execPath, [script], options).status === 1 ? 0 : 5;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Nat.host_export), nat_host_export);
