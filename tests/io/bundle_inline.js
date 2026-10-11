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
    put("ok.html", '<svg><script type="module"/></svg>\n'
      + '<script type="module">\nimport M from "./m.bend";\n'
      + "console.log(M.main());\n</script>\n");
    put("bad.html", '<p>\n<script\n type="module">\nconst x = ;\n</script>\n');
    put("repeated.html", '<!--\n<script type="module">\nconst x = ;\n'
      + '</script>\n-->\n<script type="module">\nconst x = ;\n</script>\n');
    put("multi.html", '<svg><script type="module"/></svg>\n'
      + '<!-- BEND_INLINE_START_0_ -->\n'
      + '<script type="module">\nconsole.log(1);\n</script>\n'
      + '<!--\nconst x = ;\n-->\n'
      + '<script type="module">\nconst x = ;\n</script>\n');
    if (build("ok.html").status !== 0) {
      return 1;
    }
    const out = path.join(dir, "out");
    const js = fs.readdirSync(out).find((f) => f.endsWith(".js"));
    if (run(path.join(out, js)).stdout.trim() !== "42") {
      return 2;
    }
    const bad = build("bad.html");
    if (bad.status !== 1 || !bad.stderr.includes("bad.html:4:11")) {
      return 3;
    }
    const repeated = build("repeated.html");
    if (repeated.status !== 1
      || !repeated.stderr.includes("repeated.html:7:11")) {
      return 4;
    }
    const multi = build("multi.html");
    return multi.status === 1 && multi.stderr.includes("multi.html:10:11")
      ? 0 : 5;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(CID(Bundle.inline), bundle_inline);
