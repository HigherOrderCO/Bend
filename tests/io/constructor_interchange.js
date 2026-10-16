function constructor_interchange() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-constructor-interchange-"));
  try {
    fs.writeFileSync(path.join(dir, "a.bend"), `import Base

type Counter is Data:
  Counter{count: Nat}

type Choice is Data:
  Left{count: Nat}
  Right{count: Nat}

type Flag is Data:
  Off{}
  On{}

def Counter.make() -> Counter: Counter{5n}
def Choice.left() -> Choice: Left{7n}
def Choice.right() -> Choice: Right{7n}
def Flag.off() -> Flag: Off{}
def Flag.on() -> Flag: On{}
`);
    fs.writeFileSync(path.join(dir, "b.bend"), `import Base
import ./a.bend as A

def dec(c: A.Counter) -> Nat:
  A.Counter{count} = c
  Nat.sub(count, 1n)

def score(c: A.Choice) -> Nat:
  match c:
    case A.Left{count}: (count + 1n : Nat)
    case A.Right{count}: (count + 2n : Nat)

def flag(c: A.Flag) -> U32:
  match c:
    case A.Off{}: 0
    case A.On{}: 1
`);
    const bun = typeof Bun !== "undefined";
    const command = bun ? process.execPath : "bun";
    const prefix = bun && Bun.main.startsWith("/$bunfs/")
      ? [] : [path.resolve("bend2/main.ts")];
    for (const name of ["a", "b"]) {
      const built = child.spawnSync(command,
        [...prefix, path.join(dir, name + ".bend"),
          "-o", path.join(dir, name + ".mjs")], {
          encoding: "utf8",
          env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
        });
      if (built.status !== 0) {
        throw built.error ?? new Error(built.stderr);
      }
    }
    const A = require(path.join(dir, "a.mjs")).default;
    const B = require(path.join(dir, "b.mjs")).default;
    return B.dec(A["Counter.make"]()) === 4
      && B.score(A["Choice.left"]()) === 8
      && B.score(A["Choice.right"]()) === 9
      && B.flag(A["Flag.off"]()) === 0
      && B.flag(A["Flag.on"]()) === 1 ? 0 : 1;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Host.constructor_interchange), constructor_interchange);
