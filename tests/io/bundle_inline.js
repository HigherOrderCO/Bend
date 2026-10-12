// bend-ids 1
// CID :Bundle.inline BEND_CID_Bundle_inline
// bend-ids end
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
      + '<svg><script type="module"><![CDATA[\n]]></script></svg>\n'
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
    if (multi.status !== 1 || !multi.stderr.includes("multi.html:12:11")) {
      return 5;
    }
    put(".collision.html.bend-inline-0.js", 'console.log("asset");\n');
    put("collision.html", '<script type="module" src="./.collision.html.bend-inline-0.js"></script>\n'
      + '<script type="module">import M from "./m.bend"; console.log(M.main());</script>\n');
    if (build("collision.html").status !== 0) {
      return 6;
    }
    const html = fs.readFileSync(path.join(out, "collision.html"), "utf8");
    const sources = [];
    new HTMLRewriter().on("script", {
      element(el) {
        const src = el.getAttribute("src");
        if (src) sources.push(src);
      },
    }).transform(html);
    put("run-page.mjs", sources.map(src => "await import("
      + JSON.stringify(require("node:url").pathToFileURL(path.resolve(out, src)).href) + ");").join("\n"));
    const collision = run(path.join(dir, "run-page.mjs"));
    if (collision.status !== 0 || collision.stdout.trim() !== "asset\n42") {
      return 7;
    }
    put(".external.html.bend-inline-99.js", "const broken = ;\n");
    put("external.html", '<script type="module" src="./.external.html.bend-inline-99.js"></script>\n');
    const external = build("external.html");
    return external.status === 1 && external.stderr.includes(".external.html.bend-inline-99.js:1:16")
      ? 0 : 8;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

io_eff(BEND_CID_Bundle_inline, bundle_inline);
