function clo_host_tail() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-clo-host-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

type Box is Type:
  H{f: U32 -> U32}

def mk(n: U32) -> Box:
  H{x => U32.add(x, n)}

def ap(f: U32 -> U32, x: U32) -> U32:
  f(x)
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
    const f = lib.mk(10).f;
    const own = (x) => x + 7;
    own.j = () => 99;
    const copy = Object.assign((x) => f(x) * 2, f);
    if (lib.ap(f, 1) !== 11 || lib.ap(own, 1) !== 8 || lib.ap(copy, 1) !== 22) {
      return 2;
    }
    Function.prototype.j = () => () => -1;
    try {
      return lib.ap((x) => x + 1, 1) === 2 && lib.ap(f, 2) === 12 ? 0 : 3;
    } finally {
      delete Function.prototype.j;
    }
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Clo.host_tail), clo_host_tail);
