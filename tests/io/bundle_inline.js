function bundle_inline() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-page-inline-"));
  const run = (...args) => child.spawnSync(process.execPath, args, {
    encoding: "utf8",
    env: { ...process.env, HOME: dir, BEND_ORIGIN: "http://127.0.0.1:9" },
  });
  const put = (name, text) => fs.writeFileSync(path.join(dir, name), text);
  const build = (page) => run(path.resolve("bend2/main.ts"),
    path.join(dir, page), "-o", path.join(dir, "out"));
  try {
    put("m.bend", "import Base\n\nlaw main:\n  U32\n\ndef main():\n  42\n");
    put("ok.html", '<script type="module">\nimport M from "./m.bend";\n'
      + "console.log(M.main());\n</script>\n");
    put("bad.html", '<p>\n<script\n type="module">\nconst x = ;\n</script>\n');
    if (build("ok.html").status !== 0) {
      return 1;
    }
    const out = path.join(dir, "out");
    const js = fs.readdirSync(out).find((f) => f.endsWith(".js"));
    if (run(path.join(out, js)).stdout.trim() !== "42") {
      return 2;
    }
    const bad = build("bad.html");
    return bad.status === 1 && bad.stderr.includes("bad.html:4:") ? 0 : 3;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Bundle.inline), bundle_inline);
