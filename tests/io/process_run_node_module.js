function process_run_node_module() {
  const bun = typeof Bun !== "undefined";
  const node = bun ? Bun.which("node") : process.execPath;
  if (node === null) return 0;
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-process-node-"));
  try {
    const input = path.join(dir, "main.bend");
    const output = path.join(dir, "main.cjs");
    const preload = path.join(dir, "preload.cjs");
    const marker = path.join(dir, "spawned");
    fs.writeFileSync(input, `import Base

def answer(r: Result<&1, &1, U32 & String, U32 & (String & String)>) -> Bool:
  match r:
    case Fail{error}: False{}
    case Done{(code, (out, err))}:
      Bool.and(U32.is_eq(code, 0),
        Bool.and(String.eq(out, "hello\\n"), String.eq(err, "")))

def main() -> IO(Unit):
  do IO<Unit>:
    r : Result<&1, &1, U32 & String, U32 & (String & String)> <-
      Process.run("/bin/echo", ["hello"], "", 128, 1000)
    IO.print(Bool.show(answer(r)))
`);
    fs.writeFileSync(preload, `const fs = require("node:fs");
const child = require("node:child_process");
const { isMainThread } = require("node:worker_threads");
const spawn = child.spawn;
child.spawn = function (...args) {
  if (!isMainThread) fs.writeFileSync(${JSON.stringify(marker)}, "spawn");
  return Reflect.apply(spawn, this, args);
};
`);
    const command = bun ? process.execPath : "bun";
    const prefix = bun && Bun.main.startsWith("/$bunfs/")
      ? [] : [path.resolve("bend2/main.ts")];
    const options = {
      encoding: "utf8",
      env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
    };
    const built = child.spawnSync(command,
      [...prefix, input, "-o", output], options);
    if (built.status !== 0) {
      throw built.error ?? new Error(built.stderr);
    }
    const load = "await import(" + JSON.stringify(
      require("node:url").pathToFileURL(output).href) + ")";
    const flags = [["--input-type=module"], ["--input-type", "module"]];
    for (let i = 0; i < flags.length; i += 1) {
      fs.rmSync(marker, { force: true });
      const ran = child.spawnSync(node,
        [...flags[i], "--require", preload, "-e", load],
        { ...options, timeout: 3000 });
      if (ran.error) {
        throw ran.error;
      }
      if (ran.status !== 0 || ran.stdout !== "True\n" || ran.stderr !== ""
        || !fs.existsSync(marker)) {
        return i + 1;
      }
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Host.process_run_node_module), process_run_node_module);
