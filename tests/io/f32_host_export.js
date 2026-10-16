function f32_host_export() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-f32-export-"));
  try {
    const input = path.join(dir, "lib.bend");
    const output = path.join(dir, "lib.mjs");
    fs.writeFileSync(input, `import Base

def add(a: F32, b: F32) -> F32:
  F32.add(a, b)
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
    const pairs = [[16777217, 1], [-16777217, -1], [2 ** -150, 2 ** -149],
      [1.0000000596046448, 2 ** -24], [3.5e38, -3.5e38], [-0, -0],
      [Infinity, 1], [-Infinity, 1], [NaN, 1]];
    for (const [a, b] of pairs) {
      if (!Object.is(lib.add(a, b), Math.fround(Math.fround(a) + Math.fround(b)))) {
        return 2;
      }
    }
    return lib.add(16777217)(1) === 16777216 ? 0 : 3;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

function f32_host_input() {
  return 16777217;
}

io_eff(CID(F32.host_export), f32_host_export);
io_eff(CID(F32.host_input), f32_host_input);
