// bend-ids 1
// CID :Bundle.page_input BEND_CID_Bundle_page_input
// bend-ids end
function bundle_page_input() {
  const fs = require("node:fs");
  const path = require("node:path");
  const child = require("node:child_process");
  const dir = fs.mkdtempSync(path.join(process.cwd(), ".bend-page-input-"));
  const html = '<script type="module">console.log(42);</script>\n';
  const page = path.join(dir, "page.html");
  const sub = (name) => {
    fs.mkdirSync(path.join(dir, name));
    return path.join(dir, name);
  };
  const build = (file, out) => child.spawnSync(process.execPath,
    [path.resolve("bend2/main.ts"), file, "-o", out], {
      encoding: "utf8", env: { ...process.env, HOME: dir },
    });
  const refused = (file, out) => {
    const r = build(file, out);
    return r.status === 1 && r.stderr.includes("would overwrite the page")
      && fs.readFileSync(page, "utf8") === html;
  };
  try {
    fs.writeFileSync(page, html);
    if (!refused(page, dir)) {
      return 1;
    }
    const hard = sub("hard");
    fs.linkSync(page, path.join(hard, "page.html"));
    if (!refused(page, hard)) {
      return 2;
    }
    const linked = sub("linked");
    fs.symlinkSync(page, path.join(linked, "page.html"));
    fs.symlinkSync(page, path.join(dir, "alias.html"));
    if (!refused(path.join(dir, "alias.html"), linked)) {
      return 3;
    }
    const beside = sub("beside");
    fs.symlinkSync(page, path.join(beside, "alias.html"));
    const ok = build(path.join(beside, "alias.html"), beside);
    const js = fs.readdirSync(beside).find((f) => f.endsWith(".js"));
    if (ok.status !== 0 || fs.readFileSync(page, "utf8") !== html
      || !fs.existsSync(path.join(beside, "page.html")) || js === undefined
      || child.spawnSync(process.execPath, [path.join(beside, js)],
        { encoding: "utf8" }).stdout.trim() !== "42") {
      return 4;
    }
    const none = build(path.join(dir, "none.html"), path.join(dir, "none"));
    if (none.status !== 1 || !none.stderr.includes("ModuleNotFound")) {
      return 5;
    }
    return 0;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(BEND_CID_Bundle_page_input, bundle_page_input);
