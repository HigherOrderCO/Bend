// bend-ids 1
// CID :Host.exit_status BEND_CID_Host_exit_status
// bend-ids end
function exit_status() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-exit-status-"));
  try {
    const bun = typeof Bun !== "undefined";
    const command = bun ? process.execPath : "bun";
    const prefix = bun && Bun.main.startsWith("/$bunfs/")
      ? [] : [path.resolve("bend2/main.ts")];
    const cases = [
      ["IO.exit(0)", 0, ""],
      ["IO.exit(255)", 255, ""],
      ["IO.exit(256)", 1, null],
      ["IO.exit(4294967295)", 1, null],
      ['IO.die(Unit, 7, "")', 7, "\n"],
      ['IO.die(Unit, 256, "reported")', 1, null, "reported\n"],
    ];
    for (let i = 0; i < cases.length; i += 1) {
      const [expression, status, stderr, start] = cases[i];
      const file = path.join(dir, "main.bend");
      fs.writeFileSync(file, `import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print("before")
    ${expression}
    IO.print("after")
`);
      const ran = child.spawnSync(command, [...prefix, file], {
        encoding: "utf8",
        env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
      });
      if (ran.error) {
        throw ran.error;
      }
      if (ran.status !== status || ran.stdout !== "before\n"
        || (stderr !== null && ran.stderr !== stderr)
        || (start !== undefined && !ran.stderr.startsWith(start))) {
        return i + 1;
      }
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(BEND_CID_Host_exit_status, exit_status);
