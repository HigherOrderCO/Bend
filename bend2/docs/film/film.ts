import * as fs from "node:fs";
import * as path from "node:path";

// The Bend film, cut to "A Cruel Angel's Thesis": media/kind_devil_theorem.mp4.
//   bun film.ts part i n | join | sheet t0 t1 step
// The song is not in the repo ($NERV_SONG); the sprites, the parts and the
// sheets go to .tmp/film.
const DIR  = import.meta.dir;
const TMP  = process.env.NERV_TMP ?? path.join(DIR, "../../../.tmp/film");
const SONG = process.env.NERV_SONG ?? path.join(TMP, "song.m4a");
const OUT  = process.env.NERV_OUT ?? path.join(TMP, "bend.mp4");
const S    = Number(process.env.NERV_SCALE ?? 1);
const W    = Math.round(1440 * S);
const H    = Math.round(1080 * S);
const FPS  = 24;
const END  = 89.6;
const PARTS = Number(process.env.NERV_PARTS ?? 64);

// Math
// ====

function clamp(x: number, a = 0, b = 1): number {
  return x < a ? a : x > b ? b : x;
}

// The progress of t through [a, b], clamped to [0, 1].
function prog(t: number, a: number, b: number): number {
  return clamp((t - a) / (b - a));
}

function smooth(x: number): number {
  x = clamp(x);
  return x * x * (3 - 2 * x);
}

function lerp(a: number, b: number, x: number): number {
  return a + (b - a) * x;
}

// A deterministic hash of an integer (and a seed) into [0, 1).
function hash(i: number, s = 0): number {
  let h = Math.imul(i ^ Math.imul(s + 0x9e3779b9, 0x85ebca6b), 0xc2b2ae35);
  h = Math.imul(h ^ (h >>> 15), 0x2c1b3c6d);
  h = Math.imul(h ^ (h >>> 12), 0x297a2d39);
  return ((h ^ (h >>> 15)) >>> 0) / 4294967296;
}

// Value noise with fractal octaves, in [0, 1).
function noise(x: number, y: number, s: number): number {
  const xi = Math.floor(x), yi = Math.floor(y);
  const u  = smooth(x - xi), v = smooth(y - yi);
  const k  = (a: number, b: number) => hash(a * 7919 + b * 104729, s);
  return lerp(lerp(k(xi, yi), k(xi + 1, yi), u), lerp(k(xi, yi + 1), k(xi + 1, yi + 1), u), v);
}

function fbm(x: number, y: number, s: number): number {
  let sum = 0, amp = 0.5;
  for (let o = 0; o < 5; ++o) {
    sum += amp * noise(x, y, s + o);
    x   *= 2.03;
    y   *= 2.03;
    amp *= 0.5;
  }
  return sum / 0.97;
}

// Colors
// ======

// Sampled from the source film.
type Col = [number, number, number];

const hex = (h: number): Col => [(h >> 16) / 255, ((h >> 8) & 255) / 255, (h & 255) / 255];
const BG     = hex(0x141d18);
const TXT    = hex(0xd0cfc2);  // captions
const STATE  = hex(0xc8cc98);  // statements
const DIM    = hex(0x8a9285);  // credits
const LINE   = hex(0xaeaf9e);  // diagram lines
const YEL    = hex(0xe0d060);
const CYAN   = hex(0xa7b7d4);
const ORANGE = hex(0xd07040);
const OCHRE  = hex(0xb0a67f);
const F_RED  = hex(0x80161a);
const F_OCH  = hex(0x3d2d10);
const F_GRN  = hex(0x1c3318);
const F_BLU  = hex(0x293976);
const RING   = hex(0x2050d0);
const REDL   = hex(0xd04a40);  // red labels
const GRNL   = hex(0x7fbf6a);  // green labels
const GOLD   = hex(0xe8b040);
const RED    = hex(0xd53843);
const GREEN  = hex(0x4c9a3c);
const CREAM  = hex(0xdbc7af);
const WHITE: Col = [1, 1, 1];
const BLACK: Col = [0, 0, 0];

// Canvas
// ======

// The frame, in RGB floats; every shape paints over it with an alpha.
const buf = new Float32Array(W * H * 3);

// The film base: BG, mottled by a slow noise, drawn once.
const base = new Float32Array(W * H * 3);
for (let y = 0; y < H; ++y) {
  for (let x = 0; x < W; ++x) {
    const m = 0.8 + 0.4 * fbm(x / (260 * S), y / (260 * S), 3);
    const k = (y * W + x) * 3;
    base[k] = BG[0] * m; base[k + 1] = BG[1] * m; base[k + 2] = BG[2] * m;
  }
}

function paint(i: number, c: Col, a: number): void {
  const k = i * 3;
  buf[k]     += (c[0] - buf[k])     * a;
  buf[k + 1] += (c[1] - buf[k + 1]) * a;
  buf[k + 2] += (c[2] - buf[k + 2]) * a;
}

// Fills the picture.
function fill(c: Col, a = 1): void {
  for (let i = 0; i < W * H; ++i) paint(i, c, a);
}

function rect(x: number, y: number, w: number, h: number, c: Col, a = 1): void {
  const x0 = Math.max(0, Math.round(x * S)), x1 = Math.min(W, Math.round((x + w) * S));
  const y0 = Math.max(0, Math.round(y * S)), y1 = Math.min(H, Math.round((y + h) * S));
  for (let py = y0; py < y1; ++py) {
    for (let px = x0; px < x1; ++px) paint(py * W + px, c, a);
  }
}

// A segment of width w, antialiased by its distance to each pixel.
function line(x0: number, y0: number, x1: number, y1: number, w: number, c: Col, a = 1): void {
  if (a <= 0) return;
  x0 *= S; y0 *= S; x1 *= S; y1 *= S; w *= S;
  const r  = w / 2 + 1;
  const bx = Math.max(0, Math.floor(Math.min(x0, x1) - r)), ex = Math.min(W - 1, Math.ceil(Math.max(x0, x1) + r));
  const by = Math.max(0, Math.floor(Math.min(y0, y1) - r)), ey = Math.min(H - 1, Math.ceil(Math.max(y0, y1) + r));
  const dx = x1 - x0, dy = y1 - y0, ll = dx * dx + dy * dy || 1e-9;
  for (let py = by; py <= ey; ++py) {
    for (let px = bx; px <= ex; ++px) {
      const qx = px + 0.5 - x0, qy = py + 0.5 - y0;
      const h  = clamp((qx * dx + qy * dy) / ll);
      const cx = qx - h * dx, cy = qy - h * dy;
      const cv = w / 2 + 0.5 - Math.sqrt(cx * cx + cy * cy);
      if (cv > 0) paint(py * W + px, c, a * Math.min(1, cv));
    }
  }
}

// A circle of radius r; with f < 1, only the arc from the top, clockwise, to
// the fraction f of a turn.
function ring(cx: number, cy: number, r: number, w: number, c: Col, a = 1, f = 1): void {
  if (a <= 0 || f <= 0) return;
  cx *= S; cy *= S; r *= S; w *= S;
  const e  = r + w / 2 + 1;
  const bx = Math.max(CLIP0, Math.floor(cx - e)), ex = Math.min(W - 1, CLIP1 - 1, Math.ceil(cx + e));
  const by = Math.max(0, Math.floor(cy - e)), ey = Math.min(H - 1, Math.ceil(cy + e));
  for (let py = by; py <= ey; ++py) {
    for (let px = bx; px <= ex; ++px) {
      const dx = px + 0.5 - cx, dy = py + 0.5 - cy;
      const cv = w / 2 + 0.5 - Math.abs(Math.sqrt(dx * dx + dy * dy) - r);
      if (cv <= 0) continue;
      if (f < 1 && (Math.atan2(dx, -dy) / (2 * Math.PI) + 1) % 1 > f) continue;
      paint(py * W + px, c, a * Math.min(1, cv));
    }
  }
}

function disc(cx: number, cy: number, r: number, c: Col, a = 1): void {
  if (a <= 0) return;
  cx *= S; cy *= S; r *= S;
  const bx = Math.max(CLIP0, Math.floor(cx - r - 1)), ex = Math.min(W - 1, CLIP1 - 1, Math.ceil(cx + r + 1));
  const by = Math.max(0, Math.floor(cy - r - 1)), ey = Math.min(H - 1, Math.ceil(cy + r + 1));
  for (let py = by; py <= ey; ++py) {
    for (let px = bx; px <= ex; ++px) {
      const dx = px + 0.5 - cx, dy = py + 0.5 - cy;
      const cv = r + 0.5 - Math.sqrt(dx * dx + dy * dy);
      if (cv > 0) paint(py * W + px, c, a * Math.min(1, cv));
    }
  }
}

// A filled triangle, by the sign of its three edges.
function tri(ax: number, ay: number, bx: number, by: number, cx: number, cy: number, c: Col, a = 1): void {
  const xs = [ax, bx, cx].map(v => v * S), ys = [ay, by, cy].map(v => v * S);
  const x0 = Math.max(0, Math.floor(Math.min(...xs))), x1 = Math.min(W - 1, Math.ceil(Math.max(...xs)));
  const y0 = Math.max(0, Math.floor(Math.min(...ys))), y1 = Math.min(H - 1, Math.ceil(Math.max(...ys)));
  const side = (i: number, j: number, px: number, py: number) =>
    (xs[j] - xs[i]) * (py - ys[i]) - (ys[j] - ys[i]) * (px - xs[i]);
  for (let py = y0; py <= y1; ++py) {
    for (let px = x0; px <= x1; ++px) {
      const s0 = side(0, 1, px + 0.5, py + 0.5), s1 = side(1, 2, px + 0.5, py + 0.5), s2 = side(2, 0, px + 0.5, py + 0.5);
      if ((s0 >= 0 && s1 >= 0 && s2 >= 0) || (s0 <= 0 && s1 <= 0 && s2 <= 0)) paint(py * W + px, c, a);
    }
  }
}

type Pt = [number, number];

// A polyline, drawn up to the fraction f of its length (the pen).
function poly(pts: Pt[], w: number, c: Col, a = 1, f = 1): void {
  if (a <= 0 || f <= 0 || pts.length < 2) return;
  let len = 0;
  for (let i = 1; i < pts.length; ++i) len += Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]);
  let left = len * clamp(f);
  for (let i = 1; i < pts.length && left > 0; ++i) {
    const [x0, y0] = pts[i - 1], [x1, y1] = pts[i];
    const d = Math.hypot(x1 - x0, y1 - y0), k = Math.min(1, left / (d || 1e-9));
    line(x0, y0, x0 + (x1 - x0) * k, y0 + (y1 - y0) * k, w, c, a);
    left -= d;
  }
}

// A cubic Bezier as n points.
function bez(p0: Pt, p1: Pt, p2: Pt, p3: Pt, n = 24): Pt[] {
  const out: Pt[] = [];
  for (let i = 0; i <= n; ++i) {
    const t = i / n, u = 1 - t;
    out.push([u * u * u * p0[0] + 3 * u * u * t * p1[0] + 3 * u * t * t * p2[0] + t * t * t * p3[0],
              u * u * u * p0[1] + 3 * u * u * t * p1[1] + 3 * u * t * t * p2[1] + t * t * t * p3[1]]);
  }
  return out;
}

// A triangle outline of circumradius r, its apex toward angle g (radians,
// 0 = up), drawn up to the fraction f of its perimeter.
function tri_o(cx: number, cy: number, r: number, g: number, w: number, c: Col, a = 1, f = 1): Pt[] {
  const v: Pt[] = [0, 1, 2].map(k => [cx + r * Math.sin(g + k * 2 * Math.PI / 3), cy - r * Math.cos(g + k * 2 * Math.PI / 3)] as Pt);
  poly([v[0], v[1], v[2], v[0]], w, c, a, f);
  return v;
}

// A dashed segment: n dashes, each filling the fraction on of its slot.
function dash(x0: number, y0: number, x1: number, y1: number, w: number, c: Col, a = 1, n = 12, on = 0.55): void {
  for (let i = 0; i < n; ++i) {
    const u0 = i / n, u1 = (i + on) / n;
    line(lerp(x0, x1, u0), lerp(y0, y1, u0), lerp(x0, x1, u1), lerp(y0, y1, u1), w, c, a);
  }
}

// A noise field (clouds): drawn at 1/8 size, stretched with bilinear taps.
function clouds(t: number, c0: Col, c1: Col, a: number, zoom: number, seed: number): void {
  const gw = Math.ceil(W / 8) + 2, gh = Math.ceil(H / 8) + 2;
  const g  = new Float32Array(gw * gh);
  for (let y = 0; y < gh; ++y) {
    for (let x = 0; x < gw; ++x) {
      const u = (x * 8 / S) / zoom, v = (y * 8 / S) / zoom;
      g[y * gw + x] = smooth(fbm(u + t * 0.9, v + t * 0.25, seed) * 1.3 - 0.15);
    }
  }
  for (let py = 0; py < H; ++py) {
    const gy = py / 8, y0 = Math.floor(gy), fy = gy - y0;
    for (let px = 0; px < W; ++px) {
      const gx = px / 8, x0 = Math.floor(gx), fx = gx - x0, k = y0 * gw + x0;
      const m  = lerp(lerp(g[k], g[k + 1], fx), lerp(g[k + gw], g[k + gw + 1], fx), fy);
      const i  = (py * W + px) * 3;
      for (let ch = 0; ch < 3; ++ch) buf[i + ch] += (lerp(c0[ch], c1[ch], m) - buf[i + ch]) * a;
    }
  }
}

// Sprites
// =======

// Every text is a white typst sprite, registered at load and drawn by alpha.
type Sprite = { w: number; h: number; a: Uint8Array };

const MARKUP: string[] = [];
const SPRITES: Sprite[] = [];

function str(s: string): string {
  return JSON.stringify(s);
}

function sprite(markup: string): number {
  MARKUP.push(markup);
  return MARKUP.length - 1;
}

// Loads every sprite: one typst run, one ffmpeg decode per page, cached.
function sprite_load(): void {
  fs.mkdirSync(TMP, { recursive: true });
  const src = `#set page(width: auto, height: auto, margin: 40pt, fill: none)
#set text(fill: white, top-edge: "bounds", bottom-edge: "bounds")
` + MARKUP.join("\n#pagebreak()\n") + "\n";
  const dir  = path.join(TMP, `spr-${S}`);
  const stamp = path.join(dir, "src.typ");
  if (!fs.existsSync(stamp) || fs.readFileSync(stamp, "utf8") !== src) {
    fs.mkdirSync(dir, { recursive: true });
    const tmp = path.join(dir, `next-${process.pid}.typ`);
    fs.writeFileSync(tmp, src);
    run(["typst", "compile", "--ppi", String(72 * S), tmp, path.join(dir, "{0p}.png")]);
    const n = String(MARKUP.length).length;
    for (let i = 0; i < MARKUP.length; ++i) {
      const png = path.join(dir, String(i + 1).padStart(n, "0") + ".png");
      const rgba = run(["ffmpeg", "-v", "error", "-i", png, "-f", "rawvideo", "-pix_fmt", "rgba", "-"]);
      const head = fs.readFileSync(png);
      const pw = head.readUInt32BE(16), ph = head.readUInt32BE(20);
      // Crops the page to its ink, plus 2 px: the wide page margin keeps
      // every ascender and descender, and the crop keeps anchors exact.
      let x0 = pw, y0 = ph, x1 = 0, y1 = 0;
      for (let j = 0; j < pw * ph; ++j) {
        if (rgba[j * 4 + 3] === 0) continue;
        const x = j % pw, y = (j - x) / pw;
        x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, y); y1 = Math.max(y1, y);
      }
      x0 = Math.max(0, x0 - 2); y0 = Math.max(0, y0 - 2); x1 = Math.min(pw - 1, x1 + 2); y1 = Math.min(ph - 1, y1 + 2);
      const w = x1 - x0 + 1, h = y1 - y0 + 1;
      const a = new Uint8Array(w * h);
      for (let y = 0; y < h; ++y) for (let x = 0; x < w; ++x) a[y * w + x] = rgba[((y + y0) * pw + x + x0) * 4 + 3];
      fs.writeFileSync(path.join(dir, `${i}.a`), Buffer.concat([Buffer.from(new Uint32Array([w, h]).buffer), a]));
    }
    fs.renameSync(tmp, stamp);
  }
  for (let i = 0; i < MARKUP.length; ++i) {
    const b = fs.readFileSync(path.join(dir, `${i}.a`));
    const w = b.readUInt32LE(0), h = b.readUInt32LE(4);
    SPRITES.push({ w, h, a: new Uint8Array(b.buffer, b.byteOffset + 8, w * h) });
  }
}

type Blit = { c?: Col; a?: number; ax?: number; ay?: number; z?: number; show?: number };

// Draws a sprite at (x, y): ax/ay anchor it (0 = left/top, 1 = right/bottom),
// z scales it, and show reveals its left fraction (the typewriter).
function blit(id: number, x: number, y: number, o: Blit = {}): void {
  const sp = SPRITES[id], c = o.c ?? CREAM, al = o.a ?? 1, z = o.z ?? 1;
  if (al <= 0) return;
  const w  = sp.w * z, h = sp.h * z;
  const x0 = x * S - (o.ax ?? 0) * w, y0 = y * S - (o.ay ?? 0) * h;
  const cut = x0 + w * clamp(o.show ?? 1);
  const bx = Math.max(0, Math.floor(x0)), ex = Math.min(W, Math.ceil(Math.min(x0 + w, cut)));
  const by = Math.max(0, Math.floor(y0)), ey = Math.min(H, Math.ceil(y0 + h));
  for (let py = by; py < ey; ++py) {
    const sy = (py + 0.5 - y0) / z - 0.5;
    const iy = Math.max(0, Math.min(sp.h - 1, Math.round(sy)));
    for (let px = bx; px < ex; ++px) {
      let v: number;
      if (z === 1) {
        const ix = px - Math.round(x0);
        const jy = py - Math.round(y0);
        v = ix >= 0 && ix < sp.w && jy >= 0 && jy < sp.h ? sp.a[jy * sp.w + ix] : 0;
      } else {
        const sx = (px + 0.5 - x0) / z - 0.5;
        const x1 = Math.floor(sx), y1 = Math.floor(sy), fx = sx - x1, fy = sy - y1;
        const at = (i: number, j: number) => i >= 0 && i < sp.w && j >= 0 && j < sp.h ? sp.a[j * sp.w + i] : 0;
        v = lerp(lerp(at(x1, y1), at(x1 + 1, y1), fx), lerp(at(x1, y1 + 1), at(x1 + 1, y1 + 1), fx), fy);
        void iy;
      }
      if (v > 0) paint(py * W + px, c, al * v / 255);
    }
  }
}

// Texts
// =====

// The three type systems of the opening: the
// CRT captions in a slim mono, wide tracked (sizes are pixels at 1080,
// where 1 pt is 1 px); the flash cards in a condensed bold grotesque, ALL
// CAPS, stretched to 82% of the picture's width; the staff card in a heavy
// Mincho.
function mono(s: string, size = 64, track = 0.14): number {
  return sprite(`#text(font: "Andale Mono", size: ${size}pt, tracking: ${track}em, ${str(s)})`);
}

// A flash card: cap height cap (px), the width fit to fit (px), clamped to
// an x-scale of 55-110%. With top, a small bold line sits above the word.
function flash(word: string, cap = 330, top = "", fit = 1180): number {
  const big = `text(font: "Helvetica Neue", weight: "bold", stretch: 75%, size: ${(cap / 0.714).toFixed(1)}pt, ${str(word)})`;
  const small = `text(font: "Helvetica Neue", weight: "bold", size: ${(84 / 0.714).toFixed(1)}pt, ${str(top)})`;
  return sprite(`#context {
  let b = ${big}
  let s = ${small}
  let k = calc.min(1.1, calc.max(0.55, ${fit}pt / measure(b).width))
  let j = calc.min(1, 1150pt / calc.max(1pt, measure(s).width))
  align(center, stack(dir: ttb, ${top ? "scale(x: j * 100%, reflow: true, s), v(40pt), " : ""}scale(x: k * 100%, reflow: true, b)))
}`);
}

// The inverse card (NGE's ADAM): plain Helvetica bold, drawn black on white.
function plain(word: string, cap = 300, fit = 1190): number {
  return sprite(`#context {
  let b = text(font: "Helvetica Neue", weight: "bold", size: ${(cap / 0.714).toFixed(1)}pt, ${str(word)})
  scale(x: calc.min(1.1, ${fit}pt / measure(b).width) * 100%, reflow: true, b)
}`);
}

// The small-caps card (NGE's ABSOLUTE TERROR FIELD): two words as a big
// initial and small caps, then one word in full caps; left aligned.
function smallcaps(a: string, b: string, c: string): number {
  const row = (w: string) => `[#text(font: "Helvetica Neue", weight: "bold", size: 250pt, ${str(w[0])})#text(font: "Arial", weight: "black", size: 106pt, ${str(w.slice(1))})]`;
  return sprite(`#context {
  let c = text(font: "Helvetica Neue", weight: "bold", size: 250pt, ${str(c)})
  stack(dir: ttb, spacing: 50pt, ${row(a)}, ${row(b)}, scale(x: calc.min(1, 1150pt / measure(c).width) * 100%, reflow: true, c))
}`);
}

// The badge digit and the drop cap: a bold Clarendon.
function clar(s: string, size: number): number {
  return sprite(`#text(font: "Superclarendon", weight: "bold", size: ${size}pt, ${str(s)})`);
}

// The staff card's Mincho: Songti black, stroked to Matisse weight.
function mincho(s: string, size: number, dir = "ltr"): number {
  return sprite(`#scale(x: 86%, reflow: true, stack(dir: ${dir}, ${[...s].map(g => `text(font: "Songti SC", weight: "black", size: ${size}pt, stroke: ${size * 0.02}pt + white, ${str(g)})`).join(", ")}))`);
}

// The Latin of the NGE title and credits: a black Songti serif.
function serif(s: string, size: number, track = 0.04): number {
  return sprite(`#text(font: "Songti SC", weight: "black", size: ${size}pt, tracking: ${track}em, stroke: ${size * 0.01}pt + white, ${str(s)})`);
}

// A formula, in the italic of a textbook.
function math(s: string, size: number): number {
  return sprite(`#text(font: "New Computer Modern", style: "italic", size: ${size}pt, ${str(s)})`);
}

// Figures
// =======

// The interaction net of (2 2), reduced to Church 4 in parallel: each round
// fires every active pair at once (net.json, a state per
// round). Nodes are triangles; the apex is the principal port; two apexes
// that touch are an active pair.
type NNode = { id: number; kind: string; label: number; x: number; y: number; angle: number; flip?: boolean; from?: number };
type NStep = { nodes: NNode[]; wires: { a: [number, number]; b: [number, number] }[]; active: [number, number][] };
const NET: NStep[] = JSON.parse(fs.readFileSync(path.join(DIR, "net.json"), "utf8")).steps;

// The frame of each state: its nodes' bounds, fit to a box of the picture.
function net_cam(k: number): [number, number, number] {
  const ns = NET[Math.min(k, NET.length - 1)].nodes;
  const x0 = Math.min(...ns.map(n => n.x)), x1 = Math.max(...ns.map(n => n.x));
  const y0 = Math.min(...ns.map(n => n.y)), y1 = Math.max(...ns.map(n => n.y));
  const z = Math.min(1500, 820 / Math.max(x1 - x0, 0.3), 760 / Math.max(y1 - y0, 0.3));
  return [(x0 + x1) / 2, (y0 + y1) / 2, z];
}

// A sparkle: a four-pointed star of light, s long.
function sparkle(x: number, y: number, s: number, c: Col, a: number): void {
  if (a <= 0) return;
  line(x - s, y, x + s, y, 1.6, c, a);
  line(x, y - s, x, y + s, 1.6, c, a);
  line(x - s * 0.4, y - s * 0.4, x + s * 0.4, y + s * 0.4, 1.2, c, a * 0.6);
  line(x - s * 0.4, y + s * 0.4, x + s * 0.4, y - s * 0.4, 1.2, c, a * 0.6);
  disc(x, y, 2.5, WHITE, a);
}

type NetOpt = { cy?: number; zoom?: number; c?: Col; hot?: Col; dup?: Col; a?: number; draw?: number; t?: number; calm?: number };

// Draws the net at u (fractional: round k + p). Until p = 0.4 the round's
// active pairs charge up; at 0.4 they all flare; then the rewrite: nodes
// glide to their new places, copies grow out of the pair that made them,
// dead nodes shrink away. A wire is a cubic that leaves each port along its
// way out, with a glow, and pulses of light run along it (t: seconds).
// draw < 1 draws the first state in part (the pen, in node order); calm
// (0 to 1) quiets it to a ghost: thin wires, no glow, pulses or flares.
function net(u: number, o: NetOpt = {}): void {
  const c = o.c ?? CREAM, hot = o.hot ?? GOLD, dup = o.dup ?? RED, al = o.a ?? 1, t = o.t ?? 0, vivid = 1 - (o.calm ?? 0);
  const k = Math.max(0, Math.min(NET.length - 1, Math.floor(u)));
  const p = k >= NET.length - 1 ? 0 : u - k;
  const A = NET[k], B = NET[Math.min(k + 1, NET.length - 1)];
  const m = smooth(prog(p, 0.4, 0.9)), charge = prog(p, 0, 0.4) * (1 - m) * vivid;
  const ca = net_cam(k), cb = net_cam(k + 1);
  const cam = [lerp(ca[0], cb[0], m), lerp(ca[1], cb[1], m), lerp(ca[2], cb[2], m) * (o.zoom ?? 1)];
  const X = (x: number) => 720 + (x - cam[0]) * cam[2];
  const Y = (y: number) => (o.cy ?? 540) + (y - cam[1]) * cam[2];
  const r = 0.028 * cam[2];
  const ia = new Map(A.nodes.map(n => [n.id, n])), ib = new Map(B.nodes.map(n => [n.id, n]));
  const act = new Set(A.active.flat());
  const turn = (a: number, b: number) => a + (((b - a + 3 * Math.PI) % (2 * Math.PI)) - Math.PI);
  // Where each node is now: [x, y, angle, size, flip, kind].
  const at = new Map<number, [number, number, number, number, boolean, string]>();
  const live = m > 0 ? B.nodes : A.nodes;
  for (const n of live) {
    const a = ia.get(n.id) ?? ia.get(n.from ?? -1) ?? n;
    const born = !ia.has(n.id);
    at.set(n.id, [X(lerp(a.x, n.x, m)), Y(lerp(a.y, n.y, m)), lerp(a.angle, turn(a.angle, n.angle), m), born ? m : 1, !!n.flip, n.kind]);
  }
  if (m > 0) for (const n of A.nodes) if (!ib.has(n.id)) at.set(n.id, [X(n.x), Y(n.y), n.angle, 1 - m, !!n.flip, n.kind]);
  // A port's place and its way out: the apex along the angle; the base
  // corners backward and a little outward.
  const port = (id: number, q: number): [number, number, number, number] => {
    const [x, y, g, z, f] = at.get(id)!;
    if (q === 0) return [x + r * z * Math.cos(g), y + r * z * Math.sin(g), Math.cos(g), Math.sin(g)];
    const h = g + ((q === 1) !== f ? 1 : -1) * 2 * Math.PI / 3;
    const tx = Math.cos(h) - 0.6 * Math.cos(g), ty = Math.sin(h) - 0.6 * Math.sin(g), l = Math.hypot(tx, ty);
    return [x + r * z * Math.cos(h), y + r * z * Math.sin(h), tx / l, ty / l];
  };
  const order = live.map(n => n.id);
  const pen = (id: number) => o.draw === undefined ? 1 : clamp(o.draw * order.length - order.indexOf(id));
  const wires = m > 0 ? B.wires : A.wires;
  for (const w of wires) {
    if (!at.has(w.a[0]) || !at.has(w.b[0])) continue;
    const pd = Math.min(pen(w.a[0]), pen(w.b[0]));
    if (pd <= 0) continue;
    const pa = port(w.a[0], w.a[1]), pb = port(w.b[0], w.b[1]);
    const d = Math.hypot(pb[0] - pa[0], pb[1] - pa[1]), e = Math.min(0.45 * d, 3.5 * r);
    const pts = bez([pa[0], pa[1]], [pa[0] + e * pa[2], pa[1] + e * pa[3]], [pb[0] + e * pb[2], pb[1] + e * pb[3]], [pb[0], pb[1]], 20);
    const h = act.has(w.a[0]) && act.has(w.b[0]) && w.a[1] === 0 && w.b[1] === 0 && m === 0;
    poly(pts, h ? 12 : 7, h ? hot : c, al * vivid * (h ? 0.3 * charge : 0.12), pd);
    poly(pts, h ? lerp(2.4, 3, vivid) : lerp(2.4, 2, vivid), h ? WHITE : c, al * (h ? 0.8 + 0.2 * charge : 0.85), pd);
    // A pulse of light runs along some wires.
    const s = w.a[0] * 37 + w.b[0] * 11 + w.a[1] * 3 + w.b[1];
    if (pd >= 1 && hash(s, 120) < 0.6) {
      const f = (t * (0.5 + 0.4 * hash(s, 121)) + hash(s, 122)) % 1, i = f * 20, j = Math.floor(i);
      const x = lerp(pts[j][0], pts[j + 1][0], i - j), y = lerp(pts[j][1], pts[j + 1][1], i - j);
      disc(x, y, 7, c, al * vivid * 0.2 * Math.sin(Math.PI * f));
      disc(x, y, 2.5, WHITE, al * vivid * 0.9 * Math.sin(Math.PI * f));
    }
  }
  for (const [id, [x, y, g, z, , kind]] of at) {
    const pd = pen(id);
    if (pd <= 0 || z <= 0.01) continue;
    const h = m === 0 && act.has(id);
    const col = h ? [0, 1, 2].map(i => lerp(c[i], hot[i], charge)) as Col : kind === "dup" ? dup : c;
    if (kind === "root") { ring(x, y, r * 0.6, 2.5, col, al); continue; }
    const v = [0, 1, 2].map(q => port(id, q));
    tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], kind === "dup" ? dup : c, al * pd * (kind === "dup" ? 0.75 : 0.14));
    if (h) tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], WHITE, al * 0.5 * charge);
    const tv: Pt[] = [[v[0][0], v[0][1]], [v[1][0], v[1][1]], [v[2][0], v[2][1]], [v[0][0], v[0][1]]];
    poly(tv, 7, col, al * vivid * 0.15, pd);
    poly(tv, lerp(2.8, h ? 3.5 : 2.5, vivid), col, al, pd);
    if (kind === "lam") disc(x, y, r * z * 0.14, col, al * pd);
    // Now and then, a corner twinkles.
    const tw = (t * 3 + hash(id, 123)) % 1;
    if (hash(id * 7 + Math.floor(t * 3 + hash(id, 123)), 124) < 0.12) sparkle(v[1 + (id & 1)][0], v[1 + (id & 1)][1], 12 * z, WHITE, al * vivid * pd * Math.sin(Math.PI * tw));
  }
  // The flares: every pair of the round at once, where it met.
  const q = prog(p, 0.4, 0.8);
  if (q > 0 && q < 1 && vivid > 0) for (const [i, j] of A.active) {
    const a = ia.get(i)!, b = ia.get(j)!, sx = X((a.x + b.x) / 2), sy = Y((a.y + b.y) / 2), fa = al * vivid;
    for (let l = 1; l <= 4; ++l) disc(sx, sy, r * 2.2 * l / 4 * (0.5 + q), hot, fa * 0.2 * (1 - q));
    disc(sx, sy, r * 0.7 * (1 - q), WHITE, fa);
    ring(sx, sy, r * (0.6 + 4 * q), 3 * (1 - q) + 0.5, WHITE, fa * (1 - q));
    sparkle(sx, sy, r * 5 * (1 - q * q), WHITE, fa * (1 - q));
    for (let l = 0; l < 8; ++l) {
      const g = l * Math.PI / 4 + hash(i, 125 + l), d = r * (1 + 5 * (1 - (1 - q) ** 3));
      disc(sx + d * Math.cos(g), sy + d * Math.sin(g), 3 * (1 - q), WHITE, fa);
    }
  }
}

// Bend's GPU sort (bench/runtime/tree-bitonic) on 16 keys: the fork tree
// on the left splits the work down to the 16 wires; each of the 10 stages
// is a column of 8 compare-exchanges that fire at once; the bars on the
// right are the keys, and they end as a ramp.
type BStage = { level: number; pairs: [number, number, number, boolean][]; after: number[] };
const BIT = JSON.parse(fs.readFileSync(path.join(DIR, "bitonic.json"), "utf8")) as { input: number[]; stages: BStage[] };
const BIT_Y  = (i: number) => 150 + i * 52;
const BIT_SX = BIT.stages.map((st, k) => 470 + k * 62 + (st.level - 1) * 40);

type BitOpt = { tree?: number; wires?: number; stage?: number; sweep?: number; c?: Col; hot?: Col; a?: number };

// tree/wires in [0, 1] grow those parts; stage in [0, 10] fires the stages
// up to it (the fraction animates the current one); sweep in [0, 1] runs a
// line of light down the tips of the sorted keys, each key lit as it passes.
function bitonic(o: BitOpt): void {
  const c = o.c ?? CREAM, hot = o.hot ?? GOLD, al = o.a ?? 1, st = o.stage ?? 0;
  // The fork tree: depth 0 at x = 120, the 16 leaves at x = 370.
  const tr = o.tree ?? 1;
  const node = (d: number, j: number): Pt => [120 + d * 62, (BIT_Y(j * 16 / 2 ** d) + BIT_Y((j + 1) * 16 / 2 ** d - 1)) / 2];
  const cur = Math.min(BIT.stages.length - 1, Math.floor(st));
  const lvl = st > 0 && st < BIT.stages.length ? BIT.stages[cur].level : -1;
  for (let d = 0; d < 4; ++d) {
    for (let j = 0; j < 2 ** d; ++j) {
      const g = prog(tr, d / 4, (d + 1) / 4);
      if (g <= 0) continue;
      const [x, y] = node(d, j);
      for (const q of [0, 1]) {
        const [x1, y1] = node(d + 1, 2 * j + q);
        poly([[x, y], [x + 20, y], [x + 20, y1], [x1, y1]], 2, c, al, g);
      }
      const glow = 4 - d === lvl;
      disc(x, y, glow ? 7 : 4.5, glow ? hot : c, al);
    }
  }
  // The wires, drawn from the leaves.
  const wr = o.wires ?? 1;
  for (let i = 0; i < 16; ++i) {
    const g = prog(wr, i / 32, 0.5 + i / 32);
    line(370, BIT_Y(i), lerp(370, 1150, g), BIT_Y(i), 1.6, c, al * 0.7);
    if (tr >= 1) disc(370, BIT_Y(i), 4.5, c, al);
  }
  // The comparators: a column per stage, all of its pairs at once.
  for (let k = 0; k < BIT.stages.length && k < st; ++k) {
    const f = clamp(st - k), x = BIT_SX[k];
    for (const [i, j, , sw] of BIT.stages[k].pairs) {
      const flash = k === cur && f < 1 ? 1 - smooth(prog(f, 0.2, 0.9)) : 0;
      const col = sw ? hot : c;
      line(x, BIT_Y(i), x, lerp(BIT_Y(i), BIT_Y(j), smooth(prog(f, 0, 0.25))), sw ? 3 : 2, col, al);
      disc(x, BIT_Y(i), 5, col, al);
      if (f > 0.25) disc(x, BIT_Y(j), 5, col, al);
      if (flash > 0) line(x, BIT_Y(i), x, BIT_Y(j), 10, WHITE, al * 0.5 * flash);
    }
  }
  // The keys: bars at the right end, moving to their new rows as each
  // stage fires.
  const n = BIT.stages.length, k = Math.min(n, Math.floor(st));
  const prev = k === 0 ? BIT.input : BIT.stages[k - 1].after;
  const next = k < n ? BIT.stages[k].after : prev;
  const mv = smooth(prog(st - k, 0.25, 0.75));
  if (wr >= 0.99) {
    for (let v = 0; v < 16; ++v) {
      const y = lerp(BIT_Y(prev.indexOf(v)), BIT_Y(next.indexOf(v)), mv);
      line(1170, y, 1170 + 12 + v * 12, y, 12, c, al * 0.9);
    }
  }
  const sw = o.sweep ?? 0;
  if (sw <= 0) return;
  const tip = (v: number): Pt => [1182 + v * 12, BIT_Y(v)];
  const h = sw * 17 - 1;
  for (let v = 0; v < 16; ++v) {
    const g = clamp(h - v + 1) * (0.35 + 0.65 * Math.exp(-Math.max(0, h - v) / 2.5));
    line(1170, BIT_Y(v), tip(v)[0], BIT_Y(v), 12, WHITE, al * g);
  }
  const a: Pt = [1170, BIT_Y(-0.6)], b: Pt = [1182 + 16.2 * 12, BIT_Y(15.6)];
  poly([a, b], 3, hot, al, sw);
  const hx = lerp(a[0], b[0], sw), hy = lerp(a[1], b[1], sw);
  if (sw < 1) { disc(hx, hy, 14, WHITE, al * 0.35); disc(hx, hy, 6, WHITE, al); }
}

// The Mandelbrot set (bench/runtime/mandelbrot) over [-2.15, 0.85] x
// [-1.5, 1.5]: a square of 1080 px right of the menu column (x >= MX), each
// pixel's escape time computed live. The square is k x k tasks (1 for the
// one thread); each task draws its own rows, all at once: rows is the
// fraction done, under each task's lit scan line (beam in [0, 1) is the
// beam's place on it). z zooms into a seahorse's spiral
// (SEA, -0.74328 + 0.13124i); as it does, the look turns to a glow by each pixel's
// distance to the set (its estimate from the derivative), over the pixel's
// size: sharp gold filaments at every depth.
const MX = 360, SEA: Pt = [MX + 506.4, 587.25];
type MandOpt = { k?: number; rows?: number; beam?: number; z?: number; a?: number };

function mandel(o: MandOpt): void {
  const al = o.a ?? 1, z = o.z ?? 1, k = o.k ?? 1, T = 1080 / k, cut = (o.rows ?? 1) * T;
  const x0 = Math.round(MX * S), x1 = Math.round(1440 * S);
  const glow = clamp(Math.log2(z) / 3), max = Math.round(200 + 150 * Math.log2(z)), size = 3 / (1080 * z);
  for (let py = 0; py < H; ++py) {
    const Y = (py + 0.5) / S;
    if (Y % T >= cut) continue;
    const ci = ((SEA[1] + (Y - SEA[1]) / z) / 1080 - 0.5) * 3;
    for (let px = x0; px < x1; ++px) {
      const X = (px + 0.5) / S, cr = -0.65 + ((SEA[0] + (X - SEA[0]) / z - MX) / 1080 - 0.5) * 3;
      let x = 0, y = 0, dx = 0, dy = 0, n = 0, r = 0;
      for (; n < max; ++n) {
        const t = 2 * (x * dx - y * dy) + 1;
        dy = 2 * (x * dy + y * dx); dx = t;
        const xt = x * x - y * y + cr; y = 2 * x * y + ci; x = xt;
        if ((r = x * x + y * y) > 1e8) break;
      }
      const v = n >= max ? -1 : n + 2 - Math.log2(Math.log2(r));
      let e = v < 0 ? 0 : Math.sqrt(clamp((v - 1) / 60));
      if (glow > 0 && v >= 0) {
        const g = Math.max(0, 1 - Math.log2(Math.sqrt(r) * Math.log(r) * 0.5 / Math.hypot(dx, dy) / size + 1) / 6);
        e = lerp(e, 0.1 + 0.1 * (0.5 - 0.5 * Math.cos(Math.log2(n + 1) * 2.5)) + 0.8 * g ** 3, glow);
      }
      const m = 0.12 + 1.1 * e;
      const col: Col = v < 0 ? BLACK : [lerp(GREEN[0], GOLD[0], e) * m, lerp(GREEN[1], GOLD[1], e) * m, lerp(GREEN[2], GOLD[2], e) * m];
      paint(py * W + px, col, al);
    }
  }
  // The scan lines: the rows just done glow, the current one burns, and
  // each task's beam, a hot head, runs along its own.
  if (cut < T) {
    const g = Math.min(60, cut, T / 2), hl = T * 0.074;
    for (let b = 0; b < k; ++b) {
      const y = b * T + cut;
      for (let j = 0; j < 12; ++j) rect(MX, y - g + j * g / 12, 1080, g / 12, GRNL, al * 0.03 * j);
      line(MX, y, 1440, y, 4, GRNL, al);
      line(MX, y, 1440, y, 1.5, WHITE, al * 0.9);
      for (let c = 0; c < k; ++c) {
        const x = MX + c * T + (T - hl) * (((o.beam ?? 0) + (k > 1 ? hash(b * k + c, 61) : 0)) % 1);
        line(x, y, x + hl, y, 9, GRNL, al);
        line(x + hl / 4, y, x + hl, y, 5, WHITE, al);
      }
    }
  }
  line(MX, 0, MX, 1080, 2, LINE, al * 0.22);
}

// The old mark of Higher Order Co. (higherorderco.com, 2023-24):
// an active pair on an infinity of wires (logo.json). Its loop is a
// reduction that ends where it began: the pair commutes into four nodes,
// two of them meet the erasers (the dot and the ring), the two left pass
// each other, and the lobe becomes the trunk.
const LOGO = JSON.parse(fs.readFileSync(path.join(DIR, "logo.json"), "utf8")) as
  { nodes: { pts?: Pt[]; cx?: number; cy?: number; r?: number }[]; wires: { pts: Pt[] }[] };
const LOGO_W = LOGO.wires.map(w => {
  const out: Pt[] = [w.pts[0]];
  for (let i = 0; i + 3 < w.pts.length; i += 3) out.push(...bez(w.pts[i], w.pts[i + 1], w.pts[i + 2], w.pts[i + 3], 12).slice(1));
  return out;
});
const [, , L_DOT, L_RING] = LOGO.nodes;
// The shapes of A (hollow) and B (filled) from their tips: [tip, back top, back bottom].
const L_SHAPE = [[1, 0, 2], [0, 1, 2]].map((o, i) => o.map(k => {
  const p = LOGO.nodes[i].pts!, t = p[o[0]];
  return [p[k][0] - t[0], p[k][1] - t[1]] as Pt;
}));
// The spirals into the dot and into the ring, and the two loops of the
// lobe (left, right), as through-points.
const L_CURL = [3, 4].map(i => LOGO.wires[i].pts.filter((_, j) => j % 3 === 0).slice(2));
const L_LOOP = [1, 2].map(i => LOGO.wires[i].pts.filter((_, j) => j % 3 === 0));

// A node: its tip, its turn (degrees, clockwise) and its scale.
type LNode = [number, number, number, number];
// A wire is 64 points; its second half starts past the gap where it passes
// under (none if the halves meet).
const LN = 64;

// The corners of node i (A1 A2 hollow, B1 B2 filled).
function lverts(i: number, [x, y, g, s]: LNode): Pt[] {
  const c = Math.cos(g * Math.PI / 180) * s, d = Math.sin(g * Math.PI / 180) * s;
  return L_SHAPE[i >> 1].map(([u, v]) => [x + u * c - v * d, y + u * d + v * c] as Pt);
}

// Port k of a node (0 the main at the tip, 1 and 2 the aux on its back),
// and the unit direction out of the node there.
function lport(v: Pt[], k: number): [Pt, Pt] {
  const m: Pt = [(v[1][0] + v[2][0]) / 2, (v[1][1] + v[2][1]) / 2], e = [0, 0.3, 0.62][k];
  const p: Pt = k ? [lerp(v[1][0], v[2][0], e), lerp(v[1][1], v[2][1], e)] : v[0];
  const dx = v[0][0] - m[0], dy = v[0][1] - m[1], l = (k ? -1 : 1) / (Math.hypot(dx, dy) || 1);
  return [p, [dx * l, dy * l]];
}

// A centripetal Catmull-Rom curve through q (no cusps, no overshoot), 8
// points per span; d0 and d1, if given, bend its ends out of a port.
function spline(q: Pt[], d0?: Pt, d1?: Pt): Pt[] {
  const n = q.length, out: Pt[] = [q[0]];
  const end = (p: Pt, r: Pt, d?: Pt): Pt => {
    const l = Math.hypot(r[0] - p[0], r[1] - p[1]);
    return d ? [p[0] - d[0] * l, p[1] - d[1] * l] : [2 * p[0] - r[0], 2 * p[1] - r[1]];
  };
  const at = (i: number): Pt => i < 0 ? end(q[0], q[1], d0) : i >= n ? end(q[n - 1], q[n - 2], d1) : q[i];
  for (let i = 0; i + 1 < n; ++i) {
    const p = [at(i - 1), q[i], q[i + 1], at(i + 2)], t = [0];
    for (let k = 1; k < 4; ++k) t.push(t[k - 1] + Math.max(1e-3, Math.hypot(p[k][0] - p[k - 1][0], p[k][1] - p[k - 1][1]) ** 0.5));
    for (let s = 1; s <= 8; ++s) {
      const u = lerp(t[1], t[2], s / 8);
      const L = (a: Pt, b: Pt, ta: number, tb: number): Pt => [((tb - u) * a[0] + (u - ta) * b[0]) / (tb - ta), ((tb - u) * a[1] + (u - ta) * b[1]) / (tb - ta)];
      const a1 = L(p[0], p[1], t[0], t[1]), a2 = L(p[1], p[2], t[1], t[2]), a3 = L(p[2], p[3], t[2], t[3]);
      out.push(L(L(a1, a2, t[0], t[2]), L(a2, a3, t[1], t[3]), t[1], t[2]));
    }
  }
  return out;
}

// A wire of a key: a wire of logo.json ("-" reverses it), or through-points,
// where "A1.2" is port 2 of A1, "|" the gap, "(" and ")" the left and right
// loops of the lobe, "*" the spiral into the dot and "@" the one into the
// ring.
function lwire(s: string | (Pt | string)[], v: Pt[][]): Pt[] {
  let pts: Pt[], gi: number[] = [];
  if (typeof s === "string") {
    const [t, l, r, d, b] = LOGO_W, id = s.replace("-", "");
    pts = id === "lobe" ? [...l, ...r] : id === "trunk" ? t : id === "top" ? d : b;
    if (id === "lobe") gi = [l.length - 1, l.length];
    if (s[0] === "-") { pts = [...pts].reverse(); gi = gi.map(i => pts.length - 1 - i).reverse(); }
  } else {
    const q: Pt[] = [], d: (Pt | undefined)[] = [];
    const add = (p: Pt) => { const e = q[q.length - 1]; if (!e || Math.hypot(p[0] - e[0], p[1] - e[1]) > 1) q.push(p); };
    s.forEach((x, i) => {
      if (x === "|") gi = [q.length - 1, q.length];
      else if (x === "(" || x === ")") (x === "(" ? L_LOOP[0].slice(2) : L_LOOP[1].slice(0, -2)).forEach(add);
      else if (x === "*" || x === "@") L_CURL[+(x === "@")].forEach(add);
      else if (typeof x === "string") {
        const [p, e] = lport(v[["A1", "A2", "B1", "B2"].indexOf(x.slice(0, 2))], +x[3]);
        d[+!!i] = e;
        add(p);
      } else add(x);
    });
    pts = q.length > 1 ? spline(q, d[0], d[1]) : [q[0], q[0]];
    gi = gi.map(i => i * 8);
  }
  // Resample by length, each side of the gap to half the points, so that
  // the loops of one wire match from key to key.
  const cum = [0];
  for (let i = 1; i < pts.length; ++i) cum.push(cum[i - 1] + Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1]));
  const len = cum[cum.length - 1], p: Pt[] = [], h = LN / 2;
  const span = gi.length ? [[0, cum[gi[0]]], [cum[gi[1]], len]] : [[0, len / 2], [len / 2, len]];
  for (let k = 0, j = 1; k < LN; ++k) {
    const [a, b] = span[+(k >= h)], d = lerp(a, b, (k % h) / (h - 1));
    while (j < pts.length - 1 && cum[j] < d) ++j;
    const e = clamp((d - cum[j - 1]) / (cum[j] - cum[j - 1] || 1));
    p.push([lerp(pts[j - 1][0], pts[j][0], e), lerp(pts[j - 1][1], pts[j][1], e)]);
  }
  return p;
}

// A polyline as one brush stroke: each pixel is painted once, by its
// distance to the line, and a soft glow of radius gr surrounds it; tw, if
// given, scales the width at each point.
const L_FIELD = new Float32Array(W * H);
function lstroke(pts: Pt[], w: number, c: Col, a: number, gr = 0, tw?: number[]): void {
  if (a <= 0 || pts.length < 2) return;
  const q = pts.map(([x, y]) => [x * S, y * S]), hw = w * S / 2, g = gr * S, e = hw + 1 + 2 * g;
  let x0 = W, y0 = H, x1 = 0, y1 = 0;
  for (const [x, y] of q) { x0 = Math.min(x0, x); y0 = Math.min(y0, y); x1 = Math.max(x1, x); y1 = Math.max(y1, y); }
  x0 = Math.max(CLIP0, Math.floor(x0 - e)); x1 = Math.min(W - 1, CLIP1 - 1, Math.ceil(x1 + e));
  y0 = Math.max(0, Math.floor(y0 - e)); y1 = Math.min(H - 1, Math.ceil(y1 + e));
  if (x1 < x0 || y1 < y0) return;
  // The field: the least distance to the edge of the stroke.
  for (let y = y0; y <= y1; ++y) L_FIELD.fill(e, y * W + x0, y * W + x1 + 1);
  for (let i = 1; i < q.length; ++i) {
    const [ax, ay] = q[i - 1], [bx, by] = q[i], dx = bx - ax, dy = by - ay, ll = dx * dx + dy * dy || 1e-9;
    const r0 = hw * (tw?.[i - 1] ?? 1), r1 = hw * (tw?.[i] ?? 1);
    const sx = Math.max(x0, Math.floor(Math.min(ax, bx) - e)), ex = Math.min(x1, Math.ceil(Math.max(ax, bx) + e));
    const sy = Math.max(y0, Math.floor(Math.min(ay, by) - e)), ey = Math.min(y1, Math.ceil(Math.max(ay, by) + e));
    for (let py = sy; py <= ey; ++py) {
      for (let px = sx; px <= ex; ++px) {
        const qx = px + 0.5 - ax, qy = py + 0.5 - ay, h = clamp((qx * dx + qy * dy) / ll);
        const d = Math.hypot(qx - h * dx, qy - h * dy) - lerp(r0, r1, h), k = py * W + px;
        if (d < L_FIELD[k]) L_FIELD[k] = d;
      }
    }
  }
  for (let y = y0; y <= y1; ++y) {
    for (let x = x0; x <= x1; ++x) {
      const k = y * W + x, d = L_FIELD[k];
      if (d >= e - hw) continue;
      const glow = g > 0 ? 0.3 * Math.exp(-((Math.max(0, d) / g) ** 2)) : 0;
      paint(k, c, a * Math.max(clamp(0.5 - d), glow));
    }
  }
}

// The part of an evenly spaced polyline between the fractions a and b.
function lpart(p: Pt[], a: number, b: number): Pt[] {
  const n = p.length - 1, at = (u: number): Pt => {
    const j = Math.min(n - 1, Math.floor(u * n)), e = u * n - j;
    return [lerp(p[j][0], p[j + 1][0], e), lerp(p[j][1], p[j + 1][1], e)];
  };
  return b > a ? [at(a), ...p.slice(Math.floor(a * n) + 1, Math.ceil(b * n)), at(b)] : [];
}

// The poses of the loop, at phase t: the nodes A1 A2 B1 B2; the wires T L D R
// X11 X12 X21 X22 (Xij joins Bi and Aj). At rest, A1 is A and B2 is B. The
// net rests from the last pose to the first; between them it never stops.
const L_C: Pt = [490, 497], L_M: Pt = [500, 520], L_D: Pt = [212, 225], L_R: Pt = [795, 785];
const L_GONE: LNode[] = [[800, 780, -11, 0], [225, 250, -12, 0]];
const L_POSES: { t: number; n: LNode[]; w: (string | (Pt | string)[])[] }[] = [
  // At rest: the pair meets by its tips.
  { t: 0.03,
    n: [[333, 359, 0, 1], [...L_C, 0, 0], [...L_C, 0, 0], [697, 655, 0, 1]],
    w: ["trunk", "lobe", "top", "bot", [L_C, L_C], [L_C, L_C], [L_C, L_C], [L_C, L_C]] },
  // The contact: the pair slides down the trunk into one point.
  { t: 0.19,
    n: [[505, 510, 35, 0.5], [...L_C, 35, 0], [...L_C, 35, 0], [478, 487, 35, 0.5]],
    w: [["A1.0", "B2.0"],
        ["A1.2", [320, 380], [225, 378], [166, 399], "(", "|", ")", [858, 592], [790, 655], [660, 640], "B2.1"],
        ["A1.1", [330, 335], [200, 318], "*"],
        ["B2.2", [610, 650], [760, 695], "@"],
        [L_C, L_C], [L_C, L_C], [L_C, L_C], [L_C, L_C]] },
  // Commutation: two copies of B go left, two of A go right, crossed.
  { t: 0.33,
    n: [[815, 625, 13, 0.8], [800, 735, -11, 0.8], [235, 297, -12, 0.8], [202, 400, 5, 0.85]],
    w: [[L_C, L_C],
        ["B2.0", "(", "|", ")", "A1.0"],
        ["B1.0", "*"],
        ["A2.0", "@"],
        ["A1.1", [590, 505], [470, 360], "B1.1"],
        ["B1.2", [450, 400], [560, 600], "A2.1"],
        ["B2.1", [440, 460], [600, 580], "A1.2"],
        ["B2.2", [430, 560], [560, 715], "A2.2"]] },
  // Erasure: B1 meets the dot, A2 the ring; their wires now end at the
  // erasers, and the two erasers on X12 meet and vanish.
  { t: 0.53,
    n: [[820, 625, 19, 0.95], L_GONE[0], L_GONE[1], [200, 400, 5, 0.95]],
    w: [[L_C, L_C],
        ["B2.0", "(", "|", ")", "A1.0"],
        [L_D, L_D], [L_R, L_R],
        ["A1.1", [590, 430], [460, 340], [320, 318], [200, 312], "*"],
        [L_M, L_M],
        ["B2.1", [450, 480], [590, 590], "A1.2"],
        ["B2.2", [430, 575], [560, 700], [700, 722], [800, 712], "@"]] },
  // The wrap: B2 and A1 pass each other; the lobe pulls into the trunk and
  // the wire between them wraps into the lobe.
  { t: 0.67,
    n: [[705, 505, 25, 0.8], L_GONE[0], L_GONE[1], [345, 535, 15, 0.8]],
    w: [[L_C, L_C],
        ["B2.0", [285, 445], [205, 410], "(", "|", ")", [830, 590], "A1.0"],
        [L_D, L_D], [L_R, L_R],
        ["A1.1", [500, 410], [330, 340], [200, 308], "*"],
        [L_M, L_M],
        ["B2.1", "A1.2"],
        ["B2.2", [570, 610], [700, 685], [800, 712], "@"]] },
  // At rest again: A1 is A, B2 is B.
  { t: 0.96,
    n: [[333, 359, 0, 1], L_GONE[0], L_GONE[1], [697, 655, 0, 1]],
    w: [[L_C, L_C], "-trunk", [L_D, L_D], [L_R, L_R], "top", [L_M, L_M], "-lobe", "bot"] },
];
// Each pose as one vector (the nodes, then the wires' points), and its
// slopes: a monotone cubic through the poses moves every coordinate without
// a stop or an overshoot, and holds what a pose holds.
const L_T = L_POSES.map(k => k.t);
const L_V = L_POSES.map(k => {
  const v = k.n.map((q, i) => lverts(i, q));
  return Float64Array.from([...k.n.flat(), ...k.w.flatMap(s => lwire(s, v).flat())]);
});
const L_S = L_V.map((v, i) => v.map((x, j) => {
  if (i === 0 || i === L_V.length - 1) return 0;
  const d0 = (x - L_V[i - 1][j]) / (L_T[i] - L_T[i - 1]), d1 = (L_V[i + 1][j] - x) / (L_T[i + 1] - L_T[i]);
  return d0 * d1 > 0 ? 2 * d0 * d1 / (d0 + d1) : 0;
}));
// The slots that glow from phase t on, each set fading into the next.
const L_SLOT = "T L D R X11 X12 X21 X22 A1 A2 B1 B2 dot ring".split(" ");
const L_HEAT: [number, string][] = [[0, ""], [0.09, ""], [0.19, "T A1 B2"], [0.27, "X11 X12 X21 X22 A1 A2 B1 B2"], [0.35, ""],
  [0.4, "D R X12 B1 A2 dot ring"], [0.53, "X11 X22 dot ring"], [0.62, ""], [1, ""]];
// The flashes, at phase t, of size k: the pair meets; B1 and A2 meet the
// erasers; the two erasers on X12 meet.
const L_BURST: [number, Pt, number][] = [[0.19, L_C, 1], [0.4, L_D, 0.6], [0.4, [L_RING.cx!, L_RING.cy!], 0.6], [0.53, L_M, 0.5]];

// A soft disc of light at (x, y): alpha a at the center, none at r.
function lglow(x: number, y: number, r: number, c: Col, a: number): void {
  if (a <= 0 || r <= 0) return;
  const cx = x * S, cy = y * S, rr = r * S;
  const x0 = Math.max(CLIP0, Math.floor(cx - rr)), x1 = Math.min(W - 1, CLIP1 - 1, Math.ceil(cx + rr));
  const y0 = Math.max(0, Math.floor(cy - rr)), y1 = Math.min(H - 1, Math.ceil(cy + rr));
  for (let py = y0; py <= y1; ++py) {
    for (let px = x0; px <= x1; ++px) {
      const d = Math.hypot(px + 0.5 - cx, py + 0.5 - cy) / rr;
      if (d < 1) paint(py * W + px, c, a * (1 - d) ** 2.5);
    }
  }
}

// A flash of size s px at (x, y), as the old animation drew a contact: a
// white-hot glow, and short ticks that fly out around it. k is its level,
// age the phase since the contact.
function lburst(x: number, y: number, s: number, k: number, age: number, c: Col, w: number, a: number): void {
  const hot: Col = [lerp(c[0], 1, 0.6), lerp(c[1], 1, 0.6), lerp(c[2], 1, 0.6)];
  lglow(x, y, s * 0.35 * k, hot, 0.6 * k * a);
  disc(x, y, s * 0.012 * (1 + k), WHITE, Math.min(1, 1.2 * k) * a);
  const r = s * (0.07 + 0.9 * Math.max(0, age));
  for (let i = 0; i < 14; ++i) {
    const g = (i + 0.5 * hash(i, 90)) * Math.PI / 7, r0 = r * (0.85 + 0.3 * hash(i, 92)), l = s * (0.025 + 0.02 * hash(i, 91));
    line(x + Math.cos(g) * r0, y + Math.sin(g) * r0, x + Math.cos(g) * (r0 + l), y + Math.sin(g) * (r0 + l), w, hot, k * a);
  }
}

type LogoOpt = { x?: number; y?: number; z?: number; loop?: number; pen?: number; dots?: number; nodes?: number; fill?: number; c?: Col; fc?: Col; hot?: Col; w?: number; glow?: number; a?: number };

// Draws the mark centered at (x, y), z pixels per 1000 box units, at the
// phase loop of its reduction (0 is the rest). pen draws the wires in; dots
// in [0, 1] brings the rest of each wire back as dots; nodes draws the
// outlines in; fill fills the B nodes; hot is the
// color of a contact; glow the radius of the halo around each stroke.
function logo(o: LogoOpt): void {
  const z = (o.z ?? 700) / 1000, x0 = (o.x ?? 720) - 500 * z, y0 = (o.y ?? 540) - 500 * z;
  const c = o.c ?? CREAM, hot = o.hot ?? WHITE, al = o.a ?? 1, w = o.w ?? 7, gr = o.glow ?? 0;
  const pen = o.pen ?? 1, fill = o.fill ?? 1, np = o.nodes ?? 1;
  const P = (p: Pt): Pt => [x0 + p[0] * z, y0 + p[1] * z];
  const mix = (a: Col, h: number): Col => [lerp(a[0], hot[0], h), lerp(a[1], hot[1], h), lerp(a[2], hot[2], h)];
  const ph = ((o.loop ?? 0) % 1 + 1) % 1;
  // The pose: a cubic between two poses, or the rest.
  const n = L_T.length, i = Math.max(0, Math.min(n - 2, L_T.findLastIndex(t => t <= ph)));
  const dt = L_T[i + 1] - L_T[i], x = clamp((ph - L_T[i]) / dt), x2 = x * x, x3 = x2 * x;
  const A = L_V[i], B = L_V[i + 1], SA = L_S[i], SB = L_S[i + 1];
  const v = A.map((a, j) => (2 * x3 - 3 * x2 + 1) * a + (x3 - 2 * x2 + x) * dt * SA[j] + (3 * x2 - 2 * x3) * B[j] + (x3 - x2) * dt * SB[j]);
  const wire = (q: Float64Array, k: number) => Array.from({ length: LN }, (_, j) => [q[16 + (k * LN + j) * 2], q[17 + (k * LN + j) * 2]] as Pt);
  let e = 0;
  while (L_HEAT[e + 1][0] <= ph) ++e;
  const [ta, sa] = L_HEAT[e], [tb, sb] = L_HEAT[e + 1], fh = smooth((ph - ta) / (tb - ta));
  const h = L_SLOT.map(s => lerp(+sa.split(" ").includes(s), +sb.split(" ").includes(s), fh));
  // The wires, the lobe first: it passes under.
  const ends: Pt[][] = [];
  for (const k of [1, 0, 2, 3, 4, 5, 6, 7]) {
    const p = wire(v, k).map(P);
    ends[k] = [p[0], p[LN - 1]];
    let len = 0;
    for (let j = 1; j < LN; ++j) len += Math.hypot(p[j][0] - p[j - 1][0], p[j][1] - p[j - 1][1]);
    if (len < w) continue;
    // The halves taper into the gap, as the brush did, if they part; where
    // a crossing forms or dissolves, the gap widens for the tween.
    const cut = (q: Pt[]) => Math.hypot(q[LN / 2][0] - q[LN / 2 - 1][0], q[LN / 2][1] - q[LN / 2 - 1][1]) < 1;
    const m = cut(wire(A, k)) !== cut(wire(B, k)) ? Math.round(9 * Math.sin(Math.PI * x)) : 0;
    const i0 = LN / 2 - 1 - m, i1 = LN / 2 + m, g0 = i0 / (LN - 1), g1 = i1 / (LN - 1);
    const gap = clamp(Math.hypot(p[i1][0] - p[i0][0], p[i1][1] - p[i0][1]) / (3 * w));
    const tw = (d: number) => 1 - 0.8 * gap * (1 - smooth(d / 4));
    const p0 = lpart(p, 0, Math.min(pen, g0)), p1 = lpart(p, g1, pen);
    lstroke(p0, w, mix(c, h[k]), al, gr, p0.map((_, j) => tw(i0 - j)));
    lstroke(p1, w, mix(c, h[k]), al, gr, p1.map((_, j) => tw(j)));
    if (o.dots !== undefined) for (let j = 0; j < LN; j += 3) {
      const u = j / (LN - 1);
      if (u > pen && (u < g0 || u > g1)) disc(p[j][0], p[j][1], 0.7 * w, c, al * at(o.dots, u * 0.7, u * 0.7 + 0.3));
    }
  }
  // The nodes, their outlines drawn in by np.
  for (let k = 0; k < 4; ++k) {
    const q = Array.from(v.subarray(4 * k, 4 * k + 4)) as LNode;
    if (q[3] < 0.02) continue;
    const t = lverts(k, q).map(P), col = mix(c, h[8 + k]), e = [t[0], t[1], t[2], t[0]];
    const len = [1, 2, 3].map(j => Math.hypot(e[j][0] - e[j - 1][0], e[j][1] - e[j - 1][1]));
    let left = np * (len[0] + len[1] + len[2]);
    const out: Pt[] = [e[0]];
    for (let j = 1; j < 4 && left > 0; ++j) {
      const u = Math.min(1, left / len[j - 1]);
      out.push([lerp(e[j - 1][0], e[j][0], u), lerp(e[j - 1][1], e[j][1], u)]);
      left -= len[j - 1];
    }
    if (k >= 2 && fill > 0) tri(t[0][0], t[0][1], t[1][0], t[1][1], t[2][0], t[2][1], mix(o.fc ?? c, h[8 + k]), al * fill);
    lstroke(out, w, col, al, gr);
  }
  // The erasers: the dot and the ring; two more ride X12 until they meet.
  if (np >= 1) {
    const d = P([L_DOT.cx!, L_DOT.cy!]), r = P([L_RING.cx!, L_RING.cy!]);
    lstroke([d, d], 2 * L_DOT.r! * z, mix(c, h[12]), al, gr);
    lstroke(Array.from({ length: 33 }, (_, j) => [r[0] + L_RING.r! * z * Math.cos(j * Math.PI / 16), r[1] + L_RING.r! * z * Math.sin(j * Math.PI / 16)] as Pt), w * 0.8, mix(c, h[13]), al, gr);
  }
  if (ph > 0.38 && ph < 0.53) for (const e of ends[5]) ring(e[0], e[1], 12 * z, w * 0.7, hot, al * h[5]);
  // The flashes: each swells into its contact and fades after it.
  for (const [t, q, s] of L_BURST) {
    const d = ph - t, k = s * (d < 0 ? Math.exp(-((d / 0.03) ** 2)) : Math.exp(-d / 0.06));
    if (k > 0.01) lburst(...P(q), 680 * z, k, d, c, w, al);
  }
}

// More figures
// ============

// A starfield of dust: specks that twinkle, fixed per seed.
function stars(u: number, n = 90, seed = 70, a = 1): void {
  for (let i = 0; i < n; ++i) {
    const tw = 0.4 + 0.6 * Math.abs(Math.sin(u * (1 + hash(i, seed + 3) * 3) + i));
    disc(hash(i, seed) * 1440, hash(i, seed + 1) * 1080, 1 + 2 * hash(i, seed + 2), WHITE, a * 0.6 * tw);
  }
}

// The game of the README (demos/app_win_is_bug_2d): a 12x8 torus, a room
// walled on its east and south, the flag inside, the player outside.
const GX = 216, GY = 216, CELL = 84;
const cell = (x: number, y: number): Pt => [GX + (x + 0.5) * CELL, GY + (y + 0.5) * CELL];
// The room, in the order the compass meets it; the far walls that the law
// needs once the board wraps, in pairs that mirror the room.
const WALLS: Pt[] = [[3, 0], [3, 1], [3, 2], [3, 3], [2, 3], [1, 3], [0, 3]];
const FIXES: Pt[] = [[11, 0], [0, 7], [11, 1], [1, 7], [11, 2], [2, 7], [11, 3], [3, 7]];

// b: seconds into the draft (the frame, the grid, the room, the flag); me,
// born: the player and the seconds since it came; wrap: the open edges,
// their arrows and the board's dim copies all around (the torus unrolled);
// fix, won: the seconds since the far walls came, since the player reached
// the flag; trail: the player's path and its steps so far (a dot per cell
// left behind).
type GameOpt = { b?: number; me?: Pt | null; born?: number; wrap?: number; fix?: number; won?: number; trail?: [Pt[], number] };

// A wall: a square with an X, its outline drawn to the pen, then filled.
function wall_cell(x: number, y: number, pen: number, a: number): void {
  const [cx, cy] = cell(x, y), h = CELL / 2 - 6;
  if (pen >= 1) rect(cx - h, cy - h, 2 * h, 2 * h, F_OCH, a * 0.8);
  poly([[cx - h, cy - h], [cx + h, cy - h], [cx + h, cy + h], [cx - h, cy + h], [cx - h, cy - h]], 2.5, YEL, a, pen);
  if (pen >= 1) { line(cx - h, cy - h, cx + h, cy + h, 1.5, YEL, a * 0.6); line(cx + h, cy - h, cx - h, cy + h, 1.5, YEL, a * 0.6); }
}

function game(o: GameOpt): void {
  const wr = o.wrap ?? 0, R = 12 * CELL, D = 8 * CELL, fix = o.fix ?? -1;
  // One copy of the board, (dx, dy) pixels off, drafted over b seconds:
  // the frame traced, the grid lines staggered in turns, a compass that
  // sweeps the room's extent, the room, the flag.
  const board = (dx: number, dy: number, a: number, b: number) => {
    const x0 = GX + dx, x1 = x0 + R, y0 = GY + dy, y1 = y0 + D, ox = dx / CELL, oy = dy / CELL;
    poly([[x0, y0], [x1, y0], [x1, y1], [x0, y1], [x0, y0]], 2, TXT, a * (1 - 0.6 * wr), prog(b, 0, 0.4));
    if (wr > 0) {
      for (const x of [x0, x1]) dash(x, y0, x, y1, 1.5, ALARM, a * wr * 0.6, 32);
      for (const y of [y0, y1]) dash(x0, y, x1, y, 1.5, ALARM, a * wr * 0.6, 48);
    }
    for (let i = 1; i < 8; ++i) {
      const y = y0 + i * CELL, q = prog(b, 0.3 + 0.03 * i, 0.5 + 0.03 * i);
      poly(i % 2 ? [[x0, y], [x1, y]] : [[x1, y], [x0, y]], 1.5, TXT, a * 0.28, q);
    }
    for (let j = 1; j < 12; ++j) {
      const x = x0 + j * CELL, q = prog(b, 0.5 + 0.022 * j, 0.7 + 0.022 * j);
      poly(j % 2 ? [[x, y0], [x, y1]] : [[x, y1], [x, y0]], 1.5, TXT, a * 0.28, q);
    }
    compass(x0, y0, 4 * CELL, 0, Math.PI / 2, prog(b, 0.95, 1.35), GOLD, a * (1 - prog(b, 1.5, 1.8)));
    rect(x0, y0, 3 * CELL, 3 * CELL, YEL, a * 0.06 * prog(b, 1.55, 1.75));
    WALLS.forEach(([x, y], i) => wall_cell(x + ox, y + oy, prog(b, 1.2 + 0.05 * i, 1.4 + 0.05 * i), a));
    FIXES.forEach(([x, y], i) => wall_cell(x + ox, y + oy, prog(fix - HB * (i >> 1), 0, 0.15), a));
    // The flag: a pole, then a red pennant.
    const [cx, cy] = cell(1 + ox, 1 + oy), fl = prog(b, 1.62, 1.87);
    if (fl > 0) line(cx - 14, cy + 30, cx - 14, lerp(cy + 30, cy - 32, smooth(fl / 0.6)), 3, CREAM, a);
    if (fl > 0.6) tri(cx - 14, cy - 32, cx + 26, cy - 20, cx - 14, cy - 8, RED, a * prog(fl, 0.6, 1));
  };
  if (wr > 0) for (const dy of [-D, 0, D]) for (const dx of [-R, 0, R]) if (dx || dy) board(dx, dy, 0.16 * wr, 9);
  board(0, 0, 1, o.b ?? 9);
  // The far walls are conjured: each flashes white and flares.
  FIXES.forEach(([x, y], i) => {
    const dt = fix - HB * (i >> 1), [cx, cy] = cell(x, y), h = CELL / 2 - 6;
    if (dt > 0) rect(cx - h, cy - h, 2 * h, 2 * h, WHITE, 0.7 * (1 - prog(dt, 0.1, 0.35)));
    flare(dt, cx, cy, YEL, 50);
  });
  // The open edges: thin chevrons, out at one side, in at the other.
  if (wr > 0) {
    for (let j = 0; j < 8; ++j) {
      const y = GY + (j + 0.5) * CELL;
      for (const x of [GX + R + 5, GX - 15]) poly([[x, y - 9], [x + 10, y], [x, y + 9]], 1.5, ALARM, 0.6 * wr);
    }
    for (let i = 0; i < 12; ++i) {
      const x = GX + (i + 0.5) * CELL;
      for (const y of [GY + D + 5, GY - 15]) poly([[x - 9, y], [x, y + 10], [x + 9, y]], 1.5, ALARM, 0.6 * wr);
    }
  }
  // The flag, reached: its cell burns, rings flare on the beat, a red
  // flash; the stamp EXPLOIT slams onto the board and throbs on each beat;
  // the walk is named: a counterexample.
  const won = o.won ?? -1;
  if (won >= 0) {
    const [cx, cy] = cell(1, 1);
    rect(cx - CELL / 2 + 4, cy - CELL / 2 + 4, CELL - 8, CELL - 8, ALARM, 0.45 * clamp(won / 0.1));
    for (let n = 0; n < 3; ++n) flare(won - n * 0.4674, cx, cy, ALARM, 120);
    fill(ALARM, 0.5 * (1 - prog(won, 0, 0.16)));
    const p = won % (2 * HB), z = 1 + 0.8 * (1 - smooth(prog(won, 0, 0.12))) + (won > 2 * HB ? 0.12 * Math.exp(-p / 0.1) : 0);
    blit(C_STAMP, GX + 7 * CELL, GY + 5.2 * CELL, { c: ALARM, ax: 0.5, ay: 0.5, z, a: clamp(won / 0.05) });
    const lx = GX + R - SPRITES[T_CEX].w / S, la = prog(won, 0.2, 0.3);
    label(T_CEX, lx, GY + D + 34, ALARM, la);
  }
  // The trail: a tiny dot in each cell the player left, popping in as it
  // leaves; dim on the copies.
  if (o.trail) {
    const [path, k] = o.trail;
    for (let i = 0; i + 1 < path.length; ++i) {
      const q = smooth(prog(k, i + 0.2, i + 0.6)), [cx, cy] = cell(path[i][0], path[i][1]);
      if (q > 0) for (const dy of wr > 0 ? [-D, 0, D] : [0]) for (const dx of wr > 0 ? [-R, 0, R] : [0]) {
        disc(cx + dx, cy + dy, 3.2, WHITE, 0.7 * q * (dx || dy ? 0.16 * wr : 1));
      }
    }
  }
  // The player, on the torus: bright on the board, dim on its copies.
  if (o.me) {
    const [mx, my] = o.me, born = o.born ?? 9;
    if (born < 0.25) { const [cx, cy] = cell(mx, my); ball(cx, cy, born, 1); return; }
    const x = ((mx % 12) + 12) % 12, y = GY + (my + 0.5) * CELL;
    for (const k of wr > 0 ? [-1, 0, 1] : [0]) {
      clip_x(GX + k * R, GX + (k + 1) * R, () => {
        for (const m of [-1, 0, 1]) for (const n of wr > 0 ? [-1, 0, 1] : [0]) {
          ball(GX + (x + 0.5 + 12 * (k + m)) * CELL, y + n * D, 9, k === 0 && n === 0 ? 1 : 0.16 * wr);
        }
      });
    }
  }
}

// The player on a path, k steps in (fractional); a step of more than one
// cell wraps around the board, so x may leave [0, 12).
function walk(path: Pt[], k: number): Pt {
  const i = Math.max(0, Math.min(path.length - 2, Math.floor(k))), f = smooth(k - i);
  const [a, b] = [path[i], path[i + 1]];
  const dx = Math.abs(b[0] - a[0]) > 1 ? b[0] - a[0] - 12 * Math.sign(b[0] - a[0]) : b[0] - a[0];
  return [a[0] + dx * f, lerp(a[1], b[1], f)];
}

// The paths: up to the room's east wall; out by the east edge, in by the
// west, onto the flag; east, to the far wall.
const P_COME: Pt[] = [[6, 2], [5, 2], [5, 1], [4, 1]];
const P_WRAP: Pt[] = [[4, 1], [5, 1], [6, 1], [7, 1], [8, 1], [9, 1], [10, 1], [11, 1], [0, 1], [1, 1]];
const P_FIX: Pt[]  = [[8, 1], [9, 1], [10, 1]];

// Induction, u seconds in: the naturals as equal, numbered nodes on an S
// curve, each with an arrow to the next: 0 to 5, an ellipsis, then ∞. A
// node pops in every quarter beat; the base lights on the second beat, and
// the light passes a node per half beat, through the ellipsis to ∞; the
// whole chain glows when the word comes, on the beat after.
const IND: Pt[] = Array.from({ length: 8 }, (_, i): Pt => [150 + i * 163, 490 - 110 * Math.sin((i - 3.5) * 0.9)]);
function induct(u: number): void {
  const c = hex(0x9fc890), hot = YEL, r = 50, lit = (u - 0.935) / 0.2337, done = prog(u, 2.804, 3.3);
  // The gap an arrow leaves at slot i: a node's ring, or the ellipsis.
  const e = (i: number) => i === 6 ? 44 : r + 8;
  IND.forEach(([x, y], i) => {
    const t0 = i * 0.117, g = prog(u, t0, t0 + 0.2), l = lit - i;
    if (g <= 0) return;
    // The arrow in from i - 1; the light runs along it before i lights.
    if (i > 0) {
      const [px, py] = IND[i - 1], d = Math.hypot(x - px, y - py), ux = (x - px) / d, uy = (y - py) / d;
      const a: Pt = [px + ux * e(i - 1), py + uy * e(i - 1)], b: Pt = [x - ux * e(i), y - uy * e(i)];
      const col = l >= 0 ? hot : c;
      poly([a, b], 2.5, col, 1, prog(u, t0, t0 + 0.15));
      if (g >= 1) poly([[b[0] - 14 * ux - 9 * uy, b[1] - 14 * uy + 9 * ux], b, [b[0] - 14 * ux + 9 * uy, b[1] - 14 * uy - 9 * ux]], 2.5, col, 1);
      const s = smooth(prog(l, -0.7, 0));
      if (s > 0 && s < 1) spark(lerp(a[0], b[0], s), lerp(a[1], b[1], s));
    }
    if (i === 6) {
      blit(M_NUM[6], x, y, { c: l >= 0 ? hot : c, ax: 0.5, ay: 0.5, a: g });
      return;
    }
    ring(x, y, r * (1 - (1 - g) ** 3), 2.5, c, 1);
    if (g < 1) ring(x, y, r + 80 * g, 1.5, c, 0.6 * (1 - g));
    if (l >= 0) {
      disc(x, y, r - 6, hot, 0.9 * smooth(prog(l, 0, 0.4)));
      aura(x, y, i === 7 ? 160 : 110, hot, 0.5 * (1 - prog(l, 0, 1.5)) + (done > 0 ? 0.6 * (1 - done) : 0));
      if (l < 2) ring(x, y, r + (i === 7 ? 120 : 70) * prog(l, 0, 2), 2, hot, 1 - prog(l, 0, 2));
    }
    blit(M_NUM[i], x, y, { c: l >= 0.2 ? BG : c, ax: 0.5, ay: 0.5, a: g });
  });
}

// The end plate: NGE's red-orange texture, with dark line-art glyphs whose
// strokes end in ports (small rings), like wires of a net.
function plate(u: number): void {
  clouds(u * 0.2, hex(0x5a1410), hex(0xc0502a), 1, 300, 9);
  clouds(u * 0.3, hex(0x1a0605), hex(0x802926), 0.45, 120, 11);
  const ink = hex(0x3a120c);
  for (let g = 0; g < 8; ++g) {
    const ox = 170 + (g % 4) * 300, oy = 150 + Math.floor(g / 4) * 420;
    for (let k = 0; k < 3; ++k) {
      const h = (m: number): Pt => [ox + hash(g * 31 + k * 7 + m, 90) * 230, oy + hash(g * 31 + k * 7 + m, 91) * 320];
      const pts = bez(h(0), h(1), h(2), h(3), 30);
      poly(pts, 5, ink, 0.8);
      ring(pts[30][0], pts[30][1], 10, 3, ink, 0.8);
      disc(pts[0][0], pts[0][1], 6, ink, 0.8);
    }
  }
}

// Rays: n thin lines from (cx, cy), from r0 out to r1, each its own length.
function rays(cx: number, cy: number, n: number, r0: number, r1: number, c: Col, a: number, seed = 100, g0 = 0, g1 = 2 * Math.PI): void {
  for (let i = 0; i < n; ++i) {
    const g = lerp(g0, g1, (i + hash(i, seed)) / n), r = lerp(r0, r1, 0.4 + 0.6 * hash(i, seed + 1));
    line(cx + r0 * Math.cos(g), cy + r0 * Math.sin(g), cx + r * Math.cos(g), cy + r * Math.sin(g), 1, c, a * (0.3 + 0.7 * hash(i, seed + 2)));
  }
}

// Stars that stream past, out from the middle: the camera flies on.
function streaks(u: number, n = 160, a = 1): void {
  for (let i = 0; i < n; ++i) {
    const g = hash(i, 110) * 2 * Math.PI, sp = 0.25 + hash(i, 111) * 0.5;
    const r = ((hash(i, 112) + u * sp) % 1) ** 2 * 1100;
    const x = 720 + r * Math.cos(g), y = 540 + r * Math.sin(g), l = 4 + r * 0.06;
    line(x, y, x + l * Math.cos(g), y + l * Math.sin(g), 1.5 + hash(i, 113) * 1.5, WHITE, a * clamp(r / 200));
  }
}

// title figures

// A soft haze around a sprite: faint copies of it on three circles out to
// radius r, drawn under the sprite itself.
function haze(id: number, x: number, y: number, r: number, o: Blit): void {
  for (let j = 1; j <= 3; ++j) {
    for (let k = 0; k < 8; ++k) {
      const g = (k + 0.5 * j) * Math.PI / 4;
      blit(id, x + r * j / 3 * Math.cos(g), y + r * j / 3 * Math.sin(g), { ...o, a: (o.a ?? 1) * 0.06 });
    }
  }
}

// Light with a soft falloff, from our own strokes: the shape at width w
// with a white core, under ever wider and fainter copies of itself.
const FALL: [number, number][] = [[12, 0.02], [8, 0.03], [5.5, 0.05], [3.5, 0.08], [2.2, 0.12], [1.5, 0.18]];
function gring(cx: number, cy: number, r: number, w: number, c: Col, a = 1): void {
  for (const [k, f] of FALL) ring(cx, cy, r, w * k, c, a * f);
  ring(cx, cy, r, w, c, a);
  ring(cx, cy, r, w / 3, WHITE, a * 0.7);
}
function gline(x0: number, y0: number, x1: number, y1: number, w: number, c: Col, a = 1): void {
  for (const [k, f] of FALL) line(x0, y0, x1, y1, w * k, c, a * f);
  line(x0, y0, x1, y1, w, c, a);
  line(x0, y0, x1, y1, w / 3, WHITE, a * 0.8);
}
function gdot(x: number, y: number, r: number, c: Col, a = 1): void {
  sdot(x, y, r * 5, c, a * 0.3);
  sdot(x, y, r * 2, c, a * 0.6);
  disc(x, y, r, c, a);
  disc(x, y, r / 2, WHITE, a * 0.8);
}

// A soft round light: its alpha falls off as a bell of radius r.
function sdot(x: number, y: number, r: number, c: Col, a = 1): void {
  const e = 2.5 * r, cx = x * S, cy = y * S, rs = r * S;
  const bx = Math.max(0, Math.floor(cx - e * S)), ex = Math.min(W - 1, Math.ceil(cx + e * S));
  const by = Math.max(0, Math.floor(cy - e * S)), ey = Math.min(H - 1, Math.ceil(cy + e * S));
  for (let py = by; py <= ey; ++py) {
    for (let px = bx; px <= ex; ++px) {
      const d = ((px + 0.5 - cx) ** 2 + (py + 0.5 - cy) ** 2) / (rs * rs);
      if (d < 6.25) paint(py * W + px, c, a * Math.exp(-d));
    }
  }
}

// The bars of light that rise behind ベンド (u from 1.55): each rises in
// turn, then plays on the beat like a meter, flickers, and glows, its top
// edge bright; burn (0 to 1) turns them orange and draws them down.
function bars(u: number, burn: number, kata: number): void {
  const beat = 0.4674, bt = Math.max(0, u - 1.55), kick = Math.exp(-(bt % beat) / 0.14), f = Math.floor(u * FPS);
  const c: Col = [lerp(0.25, 1, burn), lerp(0.5, 0.45, burn), lerp(1, 0.1, burn)];
  for (let i = 0; i < 24; ++i) {
    const x = 200 + i * 44 + 10 * hash(i, 90), w = 5 + 13 * hash(i, 91);
    const rise = smooth(prog(bt, 0.04 * ((i * 7) % 24) / 2.4, 0.04 * ((i * 7) % 24) / 2.4 + 0.3));
    const play = 0.55 + 0.25 * Math.sin(bt * (3 + 3 * hash(i, 92)) + 6 * hash(i, 93)) + 0.2 * kick * hash(i + Math.floor(bt / beat), 94);
    const hgt = 350 * rise * play * (0.9 + 0.1 * hash(f, i + 95)) * (1 - burn), a = kata * (1 - burn * burn);
    if (hgt < 2 || a <= 0) continue;
    const y0 = 960 - hgt;
    for (let k = 0; k < 20; ++k) {
      const y = y0 + hgt * k / 20, fo = (1 - k / 20) ** 1.5;
      rect(x - 1.5 * w, y, 4 * w, hgt / 20, c, a * 0.1 * fo);
      rect(x, y, w, hgt / 20, c, a * 0.75 * fo);
    }
    gline(x, y0, x + w, y0, 2, c, a * (0.6 + 0.4 * kick));
  }
}

// seq figures

// A formula in the math of a textbook (typst math: italic letters, upright
// digits, the spacing of relations).
function tex(s: string, size = 56): number {
  return sprite(`#text(size: ${size}pt)[$${s}$]`);
}

// An inference bar from x0 to x1 at y, drawn by the pen p from the left: a
// dot at each end, a hot dot at the pen, a glint when it lands.
function sbar(x0: number, x1: number, y: number, p: number, w: number, c: Col): void {
  if (p <= 0) return;
  const x = lerp(x0, x1, 1 - (1 - clamp(p)) ** 3);
  line(x0, y, x, y, w, c);
  disc(x0, y, w * 1.3, c);
  disc(x, y, p < 1 ? w * 2 : w * 1.3, p < 1 ? WHITE : c);
  if (p >= 1) disc(x1, y, w * 4, WHITE, 0.5 * (1 - prog(p, 1, 2.2)));
}

// The proof of ∀n. n+0 = n in the sequent calculus, as Bend computes n+0
// (on its first argument): the base 0+0 = 0 computes to 0 = 0 (refl); in
// the step, the hypothesis p+0 = p enters on the left (=L) to turn
// S(p+0) = S(p+0) into S(p+0) = S(p), which S(p)+0 computes to (conv); ind
// closes both and drops the hypothesis. Drafted top down, a bar each third
// of a beat (B); the conclusion is boxed on the third beat. The hypothesis is
// orange; the rule names are gold.
function sequent(u: number): void {
  const B = 0.4674, P = 132, m = 22, d = 0.25 * 56;
  const wd = (id: number) => SPRITES[id].w / S;
  const [ref, eql, cnv, base, all] = Q_SEQ, gap = wd(Q_RULE[0]) + 2 * m + 30;
  // The base sits left of the step, a gap past it; the ind bar spans both.
  const bx = -wd(cnv) / 2 - gap - wd(base) / 2, L = bx - wd(base) / 2 - m, R = wd(cnv) / 2 + m;
  const ox = 720 - (L + R + wd(Q_RULE[3]) + 14) / 2, cx = (L + R) / 2;
  const oy = 360 + 70 * (1 - at(u, 0, 4 * B / 3));
  const Y = [oy, oy + P, oy + 2 * P, oy + 3 * P + 24];
  // sequent, center x, row, bar half width, rule, drop to its baseline, time
  const N: [number, number, number, number, number, number, number][] = [
    [ref, 0, 0, wd(ref) / 2 + m, 0, d, 0],
    [eql, 0, 1, Math.max(wd(ref), wd(eql)) / 2 + m, 1, d, B / 3],
    [cnv, 0, 2, Math.max(wd(eql), wd(cnv)) / 2 + m, 2, d, 2 * B / 3],
    [base, bx, 2, wd(base) / 2 + m, 0, 0, B],
    [all, cx, 3, (R - L) / 2, 3, 0, 4 * B / 3],
  ];
  const fih = (wd(Q_IH) + 8) / wd(eql);
  const box = at(u, 2 * B, 2 * B + 0.12), flash = 1 - prog(u, 2 * B, 2 * B + 0.3);
  N.forEach(([id, x, r, h, rule, dr, t], i) => {
    if (u < t) return;
    x += ox;
    const y = Y[r], by = y - P / 2 - (r === 3 ? 32 : 15), show = prog(u, t + 0.05, t + 0.2);
    sbar(x - h, x + h, by, (u - t) / 0.12, r === 3 ? 4 : 3, CREAM);
    blit(Q_RULE[rule], x + h + 14, by, { c: GOLD, ay: 0.5, a: at(u, t + 0.06, t + 0.14) });
    if (i === 4 && box > 0) {
      const w = wd(id) / 2 + 30, top = y - SPRITES[id].h / S - 18, bot = y + 22;
      rect(x - w, top, 2 * w, bot - top, F_RED, 0.85 * box);
      rect(x - w, top, 2 * w, bot - top, WHITE, 0.6 * flash * box);
      poly([[x - w, top], [x + w, top], [x + w, bot], [x - w, bot], [x - w, top]], 3, GOLD, 1, box);
      rect(x + w + 16, bot - 26, 20, 26, GOLD, prog(u, 2 * B + 0.1, 2 * B + 0.16));
      ring(x, (top + bot) / 2, w + 300 * (1 - flash), 3, WHITE, 0.6 * flash);
    }
    blit(id, x, y + dr, { c: i === 4 && box > 0 ? WHITE : TXT, ax: 0.5, ay: 1, show });
    if (i === 1 || i === 2) blit(id, x, y + dr, { c: ORANGE, ax: 0.5, ay: 1, show: Math.min(show, fih) });
  });
}

// gpu figures
// A perspective camera at eye, looking at at, y up, of focal length fl
// (px): it maps a world point into the picture, or to null behind the eye.
type V3 = [number, number, number];
function cam3(eye: V3, at: V3, fl: number): (p: V3) => Pt | null {
  const f = [at[0] - eye[0], at[1] - eye[1], at[2] - eye[2]], fn = Math.hypot(...f);
  const fw = f.map(v => v / fn), rn = Math.hypot(fw[2], fw[0]);
  const r = [fw[2] / rn, 0, -fw[0] / rn];
  const up = [fw[1] * r[2] - fw[2] * r[1], fw[2] * r[0] - fw[0] * r[2], fw[0] * r[1] - fw[1] * r[0]];
  return p => {
    const d = [p[0] - eye[0], p[1] - eye[1], p[2] - eye[2]];
    const z = d[0] * fw[0] + d[1] * fw[1] + d[2] * fw[2];
    if (z < 0.5) return null;
    return [720 + fl * (d[0] * r[0] + d[1] * r[1] + d[2] * r[2]) / z, 540 - fl * (d[0] * up[0] + d[1] * up[1] + d[2] * up[2]) / z];
  };
}

const blend = (a: Col, b: Col, k: number): Col => [lerp(a[0], b[0], k), lerp(a[1], b[1], k), lerp(a[2], b[2], k)];

// pow2 on the GPU, in 3D: one task at the top forks in two, twelve times;
// each fork halves its task's patch of the floor, across x then z, so the
// 4,096 leaves land one per core of a 64x64 grid. Node (d, i) sits over
// the centre of its patch, at the height of its depth.
const SP_Y = (d: number) => 46 * (1 - d / 12) ** 1.35 + 1.2;
function sp_at(d: number, i: number): V3 {
  const nx = 2 ** Math.ceil(d / 2), nz = 2 ** Math.floor(d / 2);
  let xi = 0, zi = 0;
  for (let k = 0; k < d; ++k) { const b = (i >> (d - 1 - k)) & 1; if (k % 2 === 0) xi = xi * 2 + b; else zi = zi * 2 + b; }
  return [-32 + (xi + 0.5) * 64 / nx, SP_Y(d), -32 + (zi + 0.5) * 64 / nz];
}

// q in [0, 12] sends the fork signal down, a level per unit: a bright
// pulse runs down each edge, the edge lit behind it, and each node flashes
// as the pulse lands and forks. lit in [0, 1] drops each leaf's task onto
// its core, in a scatter, and past 1 flashes the whole grid once; join in
// [0, 1] pulses the results back up to the root; cam in [0, 1] is the one
// camera move.
function split3d(q: number, lit: number, join: number, cam: number): void {
  const e = smooth(cam), g = lerp(-0.5, 0.25, e);
  const P = cam3([100 * Math.sin(g), lerp(80, 58, e), -100 * Math.cos(g)], [0, lerp(26, 14, e), 0], lerp(1500, 1120, e));
  // The floor: the patches of the forks so far, each split line drawn out
  // from its centre as its level lands; then the cores.
  const k = Math.floor(q);
  for (let d = 1; d <= Math.min(12, k + 1); ++d) {
    const f = d <= k ? 1 : smooth(q - k), ax = d % 2 === 1;
    const lines = 2 ** (ax ? Math.ceil(d / 2) : Math.floor(d / 2)), segs = f < 1 ? 2 ** (ax ? Math.floor(d / 2) : Math.ceil(d / 2)) : 1;
    const al = (d < 5 ? 0.5 : 0.22) + 0.6 * (1 - f), w = d < 5 ? 2 : 1;
    for (let j = 1; j < lines; j += 2) {
      const s = -32 + j * 64 / lines;
      for (let m = 0; m < segs; ++m) {
        const c = -32 + (m + 0.5) * 64 / segs, h = 32 / segs * f;
        const a = P(ax ? [s, 0, c - h] : [c - h, 0, s]), b = P(ax ? [s, 0, c + h] : [c + h, 0, s]);
        if (a && b) line(a[0], a[1], b[0], b[1], w, f < 1 ? WHITE : LINE, al);
      }
    }
  }
  const edge: V3[] = [[-32, 0, -32], [32, 0, -32], [32, 0, 32], [-32, 0, 32], [-32, 0, -32]];
  poly(edge.map(p => P(p)!), 2.5, TXT, 0.8);
  const all = lit >= 1 ? Math.exp(-(lit - 1) * 4.7) : 0;
  for (let i = 0; i < 4096 && lit > 0; ++i) {
    const t = lit * 1.25 - hash(i, 950) * 0.25;
    if (t <= 0) continue;
    const [x, , z] = sp_at(12, i), v = [P([x - 0.3, 0, z - 0.3]), P([x + 0.3, 0, z - 0.3]), P([x + 0.3, 0, z + 0.3]), P([x - 0.3, 0, z + 0.3])];
    if (v.some(p => !p)) continue;
    const [a, b, c, dd] = v as Pt[], hot = Math.max(Math.exp(-t / 0.06), 0.7 * all);
    tri(a[0], a[1], b[0], b[1], c[0], c[1], blend(RING, WHITE, hot), 0.55 + 0.4 * hot);
    tri(a[0], a[1], c[0], c[1], dd[0], dd[1], blend(RING, WHITE, hot), 0.55 + 0.4 * hot);
  }
  // The edges, cyan at the top to blue at the floor: lit up to the pulse,
  // whiter just behind it; the join runs a second pulse up from the
  // leaves, leaving the edges pale.
  const jq = join * 12;
  for (let d = 0; d < 12 && d < q; ++d) {
    const f = clamp(q - d), h = clamp(jq - (11 - d)), w = d < 3 ? 2.6 : d < 7 ? 1.8 : 1.1, al = d < 8 ? 0.9 : 0.55;
    const hr = Math.max(1.4, 6 - d * 0.45), ec = blend(CYAN, RING, (d / 11) ** 1.5);
    for (let i = 0; i < 2 ** d; ++i) {
      const pa = P(sp_at(d, i));
      if (!pa) continue;
      for (const b of [0, 1]) {
        const cb = P(sp_at(d + 1, 2 * i + b));
        if (!cb) continue;
        const at = (t: number): Pt => [lerp(pa[0], cb[0], t), lerp(pa[1], cb[1], t)];
        const [hx, hy] = at(f), [tx, ty] = at(Math.max(0, f - 0.35));
        line(pa[0], pa[1], hx, hy, w, h > 0 ? TXT : ec, al);
        if (f < 1) {
          line(tx, ty, hx, hy, w + 0.8, WHITE, al);
          if (d < 9) disc(hx, hy, hr * 2.2, WHITE, 0.2);
          disc(hx, hy, hr, WHITE, 1);
        }
        if (h > 0 && h < 1) {
          const [ux, uy] = at(1 - h);
          line(ux, uy, cb[0], cb[1], w + 0.8, WHITE, al);
          disc(ux, uy, hr, WHITE, 1);
        }
      }
    }
  }
  // The nodes: each flashes as the signal lands (or the join passes), then
  // stays a dot, down to depth 8.
  for (let d = 1; d <= 12 && d <= q; ++d) {
    const age = q - d, up = jq - (12 - d), r0 = Math.max(1.2, 5 - d * 0.35);
    const fl = Math.max(age < 0.8 ? 1 - age / 0.8 : 0, up > 0 && up < 0.8 ? 1 - up / 0.8 : 0);
    if (fl <= 0 && d > 8) continue;
    for (let i = 0; i < 2 ** d; ++i) {
      const c = P(sp_at(d, i));
      if (!c) continue;
      if (d <= 8) disc(c[0], c[1], r0 * 0.7, blend(CYAN, RING, d / 12), 0.9);
      if (fl <= 0) continue;
      disc(c[0], c[1], r0 * (1 + fl), WHITE, fl);
      if (d < 6) ring(c[0], c[1], r0 * (1.5 + 5 * (1 - fl)), 1.5, CYAN, fl);
    }
  }
  const r = P(sp_at(0, 0));
  if (!r) return;
  const rf = Math.max(1 - clamp(q), join >= 1 ? 1 : 0);
  disc(r[0], r[1], 7, WHITE, 1);
  ring(r[0], r[1], 13, 2, CYAN, 0.9);
  ring(r[0], r[1], 16 + 40 * (1 - rf), 2, WHITE, rf);
  blit(T_POW2, r[0] + 26, r[1], { c: TXT, ay: 0.5 });
}

// logo figures

// The eyes open on the red sea (o from 0 to 1): the light comes up in the
// middle first and reaches the top and the bottom last, the lids curved,
// so the frame dims by its distance d from the middle line.
function lids(o: number): void {
  if (o >= 1) return;
  for (let py = 0; py < H; ++py) {
    const dy = Math.abs(py / S - 540) / 540;
    for (let px = 0; px < W; ++px) {
      const dx = (px / S - 720) / 720, d = dy + 0.15 * dx * dx;
      const b = smooth(clamp((1.8 * o - d) / 0.8)), k = (py * W + px) * 3;
      buf[k] *= b; buf[k + 1] *= b; buf[k + 2] *= b;
    }
  }
}

// board figures

// The verse's beat grid, 37.285 + 0.4674 j, and its half beat: the game
// steps on it.
const gbeat = (j: number) => 37.285 + 0.4674 * j;
const HB = 0.4674 / 2;
const ALARM = hex(0xff453a);

// A compass: the arc about (cx, cy) of radius r from angle g0 to g1
// (radians, 0 = east, y down), swept to the fraction f, its arm out while
// it sweeps.
function compass(cx: number, cy: number, r: number, g0: number, g1: number, f: number, c: Col, a = 1, w = 2.5): void {
  if (f <= 0 || a <= 0) return;
  const pts = Array.from({ length: 41 }, (_, i): Pt => [cx + r * Math.cos(lerp(g0, g1, i / 40)), cy + r * Math.sin(lerp(g0, g1, i / 40))]);
  poly(pts, w, c, a, f);
  if (f < 1) {
    const g = lerp(g0, g1, f);
    line(cx, cy, cx + r * Math.cos(g), cy + r * Math.sin(g), 1.5, c, a * 0.5);
    disc(cx, cy, 4, c, a);
  }
}

// Rings that flare out of (x, y), dt seconds after a hit.
function flare(dt: number, x: number, y: number, c: Col, r = 70): void {
  for (let i = 0; i < 3; ++i) {
    const p = prog(dt - 0.07 * i, 0, 0.45);
    if (p > 0 && p < 1) ring(x, y, 10 + r * (1 - (1 - p) ** 3), 3, c, 1 - p);
  }
}

// The player: a ball whose outline a compass sweeps in its first 0.25 s.
function ball(x: number, y: number, born: number, a: number): void {
  if (born < 0.25) { compass(x, y, 20, -Math.PI / 2, 1.5 * Math.PI, born / 0.25, CYAN, a, 3); return; }
  disc(x, y, 34, CYAN, a * 0.1);
  disc(x, y, 26, CYAN, a * 0.16);
  disc(x, y, 20, CYAN, a * 0.55);
  ring(x, y, 20, 3, CYAN, a);
  disc(x, y, 7, WHITE, a * 0.9);
}

// A lunge from cell p toward d: the ball touches the wall dt = 0, recoils.
function lunge(p: Pt, d: Pt, dt: number): Pt {
  const k = 0.33 * (dt < 0 ? prog(dt, -0.14, 0) ** 2 : 1 - smooth(prog(dt, 0, 0.2)));
  return [p[0] + d[0] * k, p[1] + d[1] * k];
}

// A bump, dt seconds after the ball hit the wall's face (x0, y0)-(x1, y1):
// the face burns red, rings flare out of its middle.
function bump(dt: number, x0: number, y0: number, x1: number, y1: number): void {
  if (dt >= 0 && dt < 0.25) line(x0, y0, x1, y1, 6, ALARM, 1 - prog(dt, 0.1, 0.25));
  flare(dt, (x0 + x1) / 2, (y0 + y1) / 2, ALARM, 110);
}

// The captions: one line close above the board, on its left edge, over a
// dark haze of its own shape, so it reads over the board's copies; ADV is
// the mono's advance.
const CAPX = GX, CAPB = GY - 36, ADV = 54 * 0.66;
function cap(id: number, o: Blit = {}): void {
  haze(id, CAPX, CAPB, 10, { ...o, c: BG, ay: 1, a: 12 });
  blit(id, CAPX, CAPB, { c: TXT, ay: 1, ...o });
}

// A label at (x, y), top left, over the same haze.
function label(id: number, x: number, y: number, c: Col, a: number): void {
  haze(id, x, y, 8, { c: BG, a: 12 * a });
  blit(id, x, y, { c, a });
}

// The law's caption, typed to the fraction f, a block cursor at the pen;
// "LAW:" burns gold, red while the law is broken (bad, 0 to 1). dt seconds
// after the law holds, a big check swept by a compass in the board's
// middle, the law's formula under it.
function law_cap(f: number, bad: number, dt: number): void {
  const w = SPRITES[T_LAW].w / S, h = SPRITES[T_LAW].h / S;
  cap(T_LAW, { show: f });
  blit(T_LAWW, CAPX, CAPB - h, { c: bad > 0 ? ALARM : YEL, show: f * w / (SPRITES[T_LAWW].w / S) });
  if (f > 0 && f < 1) rect(CAPX + f * w, CAPB - h, ADV * 0.8, h, TXT, 0.85);
  const [x, y] = [GX + 6 * CELL, GY + 4 * CELL];
  disc(x, y, 76, BG, 0.85 * prog(dt, 0, 0.1));
  compass(x, y, 72, -Math.PI / 2, 1.5 * Math.PI, prog(dt, 0, 0.2), GRNL, 1, 4);
  poly([[x - 34, y + 2], [x - 10, y + 26], [x + 36, y - 28]], 7, GRNL, 1, prog(dt, 0.14, 0.3));
  flare(dt - 0.14, x, y, GRNL, 170);
  const lx = x - SPRITES[T_MOVES].w / S / 2, la = prog(dt, 0.3, 0.4);
  label(T_MOVES, lx, y + 110, GRNL, la);
}

// proof figures

// The question's net, a row of A::B tokens: each node points left or right
// (its apex, the principal port, is the # side) and is wired to its
// neighbours: #A A# #A A# #B B# (B in red). Node i pops in at t[i]. At f
// both facing pairs fire: A# #A annihilate, A# #B swap (they pass through
// each other); at f + 1 beat the row, #A #B A# B#, spreads out: no pair
// faces, the normal form.
const ASK_DIR = [-1, 1, -1, 1, -1, 1];
function ask_net(u: number, t: number[], f: number): void {
  const y0 = 545, r = 56, B = 0.4674, X = (i: number) => 170 + i * 220;
  const an = smooth(prog(u, f, f + 0.3)), sw = smooth(prog(u, f, f + 0.35)), st = smooth(prog(u, f + B, f + B + 0.35));
  // Where node i is: [x, y, scale]; after the rewrite, the four left spread
  // evenly, in their new order.
  const at = (i: number): [number, number, number] => {
    let x = X(i), y = y0, s = 1;
    if (i === 1 || i === 2) { x = lerp(x, 500, an); s = 1 - an; }
    if (i === 3 || i === 4) { x = lerp(x, X(7 - i), sw); y = y0 + (i === 3 ? -70 : 70) * Math.sin(Math.PI * sw); }
    const j = [0, -1, -1, 2, 1, 3][i];
    if (j >= 0) x = lerp(x, 170 + j * 1100 / 3, st);
    return [x, y, s];
  };
  const live = [0, 1, 2, 3, 4, 5].filter(i => u >= t[i] && at(i)[2] > 0.01).sort((a, b) => at(a)[0] - at(b)[0]);
  // The port of node i on side d (1 = right): its apex, or its back.
  const port = (i: number, d: number): Pt => { const [x, y, s] = at(i); return [x + d * s * (ASK_DIR[i] === d ? r : r / 2), y]; };
  live.forEach((i, k) => {
    if (i === 0) poly([port(0, -1), [60, y0]], 2.5, LINE, 1, prog(u, t[0], t[0] + 0.15));
    if (i === 5) poly([port(5, 1), [1380, y0]], 2.5, LINE, 1, prog(u, t[5], t[5] + 0.15));
    if (k === 0) return;
    const h = live[k - 1], hot = ASK_DIR[h] === 1 && ASK_DIR[i] === -1 && u < f + 0.1;
    const blink = hot && u >= f - 0.06 && Math.floor(u * 24) % 2 === 0;
    poly([port(h, 1), port(i, -1)], blink ? 6 : hot ? 4 : 2.5, blink ? WHITE : hot ? YEL : LINE, 1, prog(u, Math.max(t[h], t[i]), Math.max(t[h], t[i]) + 0.15));
  });
  for (const i of live) {
    const [x, y, s] = at(i), z = r * s * (1 + 0.3 * (1 - smooth(prog(u, t[i], t[i] + 0.12))));
    const v = tri_o(x, y, z, ASK_DIR[i] * Math.PI / 2, 3, i >= 4 ? RED : LINE);
    if (i >= 4) tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], RED, 0.6);
    tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], WHITE, 0.8 * (1 - prog(u, t[i], t[i] + 0.25)));
  }
  // The swap's crossing, and the annihilation's burst.
  aura(940, y0, 90, WHITE, 0.7 * (1 - Math.abs(prog(u, f + 0.05, f + 0.3) * 2 - 1)) * (u > f + 0.05 && u < f + 0.3 ? 1 : 0));
  const b = u - f - 0.3;
  if (b >= 0 && b < 0.45) {
    const p = b / 0.45;
    aura(500, y0, 60 + 80 * p, YEL, 0.8 * (1 - p));
    for (let k = 0; k < 8; ++k) {
      const g = (k + 0.5) * Math.PI / 4, r0 = 20 + 90 * p, r1 = 40 + 150 * (1 - (1 - p) ** 3);
      line(500 + r0 * Math.cos(g), y0 + r0 * Math.sin(g), 500 + r1 * Math.cos(g), y0 + r1 * Math.sin(g), 2, YEL, 1 - p);
    }
  }
  // The normal form: a signal runs the wire end to end.
  const q = prog(u, f + 2 * B, f + 2 * B + 0.7);
  if (q > 0 && q < 1) spark(lerp(60, 1380, smooth(q)), y0);
}

// A soft glow of color c: stacked discs.
function aura(x: number, y: number, r: number, c: Col, a: number): void {
  if (a <= 0) return;
  for (let i = 1; i <= 6; ++i) disc(x, y, r * i / 6, c, a * 0.2);
}

// A spark: a white point in its glow.
function spark(x: number, y: number, a = 1): void {
  aura(x, y, 28, WHITE, 0.7 * a);
  disc(x, y, 5, WHITE, a);
}

// The point at the fraction f of a polyline's length.
function along(pts: Pt[], f: number): Pt {
  const d = pts.slice(1).map((p, i) => Math.hypot(p[0] - pts[i][0], p[1] - pts[i][1]));
  let left = clamp(f) * d.reduce((a, b) => a + b, 0);
  for (let i = 0; i < d.length; ++i, left -= d[i - 1]) {
    if (left <= d[i]) return [lerp(pts[i][0], pts[i + 1][0], left / d[i]), lerp(pts[i][1], pts[i + 1][1], left / d[i])];
  }
  return pts[pts.length - 1];
}

// Rings that flare out of (x, y), dt seconds after a hit.
function flares(dt: number, x: number, y: number, c: Col, r = 80): void {
  for (let i = 0; i < 3; ++i) {
    const p = prog(dt - 0.07 * i, 0, 0.45);
    if (p > 0 && p < 1) ring(x, y, 10 + r * (1 - (1 - p) ** 3), 3, c, 1 - p);
  }
}

// A tactic's name, centered at the bottom; with flash, the cut flashes its
// color.
function tac_name(id: number, u: number, c: Col, flash = true): void {
  if (u < 0) return;
  if (flash) fill(c, 0.3 * (1 - prog(u, 0, 0.12)));
  blit(id, 720, 870, { c, ax: 0.5, a: smooth(prog(u, 0, 0.1)) });
}

// Case analysis, u seconds in: the goal n forks into Bend's two cases, 0n
// and 1n+p; a spark runs each branch down to its case, and a tombstone
// closes it, on the beat.
function cases(u: number): void {
  const c = CYAN, fy = 380, by = 480;
  blit(M_GOAL, 720, 240, { c: u >= 0.935 ? CREAM : c, ax: 0.5, ay: 0.5 });
  poly([[720, 310], [720, fy]], 3, c, 1, prog(u, 0, 0.1));
  [340, 1100].forEach((x, i) => {
    const a = 0.08 + 0.06 * i, p = smooth(prog(u, a, a + 0.22)), br: Pt[] = [[720, fy], [x, fy], [x, by]];
    poly(br, 3, c, 1, p);
    if (p > 0 && p < 1) spark(...along(br, p));
    blit(T_CASE[i], i ? x - 24 : x + 24, fy + 18, { c, ax: i, a: prog(u, a + 0.1, a + 0.2) });
    const o = smooth(prog(u, a + 0.2, a + 0.35));
    blit(M_CASE[i], x, 600 + 14 * (1 - o), { c: YEL, ax: 0.5, ay: 0.5, a: o });
    // The tombstone: its square drawn, then filled.
    const dt = u - 0.701 - 0.234 * i, z = 44, y = 750;
    if (dt >= 0) {
      poly([[x - z / 2, y - z / 2], [x + z / 2, y - z / 2], [x + z / 2, y + z / 2], [x - z / 2, y + z / 2], [x - z / 2, y - z / 2]], 3, YEL, 1, prog(dt, 0, 0.12));
      rect(x - z / 2, y - z / 2, z, z, YEL, prog(dt, 0.12, 0.2));
      aura(x, y, 90, YEL, 0.5 * (1 - prog(dt, 0, 0.35)));
      flares(dt - 0.08, x, y, YEL);
    }
  });
}

// Reflexivity, u seconds in: a term (a small tree) and its mirror image
// draw on either side of a dashed axis, slide in and meet on the beat (an
// equals sign forms between them); then both sides compute in lockstep,
// sparks running their edges root to leaves, and on the next beat Bend's
// {==} closes the equation.
const LAV = hex(0xb89ce0);
const TN: Pt[] = [[0, 0], [-100, 150], [90, 130], [20, 280], [165, 270]];
const TE: [number, number][] = [[0, 1], [0, 2], [2, 3], [2, 4]];
function refl(u: number): void {
  const c = LAV, cx = 720, cy = 450, ax = prog(u, 0, 0.2);
  for (let k = 0; k < 16; ++k) {
    const y0 = 150 + 40 * k;
    if ((y0 - 150) / 620 < ax) line(cx, y0, cx, Math.min(y0 + 22, 150 + 620 * ax), 2, c, 0.4);
  }
  const slide = 150 * smooth(prog(u, 0.2, 0.467)), meet = u >= 0.467, done = u >= 0.935;
  for (const m of [-1, 1]) {
    const pt = (i: number): Pt => [cx + m * (-400 + slide + TN[i][0]), 330 + TN[i][1]];
    TE.forEach(([a, b], j) => {
      // Each edge's spark: the root's two edges first, then the lower two.
      const s0 = j < 2 ? 0.55 : 0.745, s = prog(u, s0, s0 + 0.19);
      poly([pt(a), pt(b)], 3, meet ? YEL : c, 1, prog(u, 0.05 + 0.04 * j, 0.15 + 0.04 * j));
      if (s >= 1) poly([pt(a), pt(b)], 3, WHITE, 0.5 * (1 - prog(u, 0.935, 1.2)));
      if (s > 0 && s < 1) spark(...along([pt(a), pt(b)], smooth(s)));
    });
    TN.forEach((_, i) => {
      const o = prog(u, 0.03 + 0.03 * i, 0.12 + 0.03 * i), [x, y] = pt(i);
      disc(x, y, 11, meet ? YEL : c, o);
      disc(x, y, 5, BG, o);
    });
  }
  if (meet) {
    const p = 1 - (1 - prog(u, 0.467, 0.6)) ** 3;
    for (const dy of [-20, 20]) line(cx - 60 * p, cy + dy, cx + 60 * p, cy + dy, 6, YEL, 1);
    aura(cx, cy, 170, WHITE, 0.7 * (1 - prog(u, 0.467, 0.8)));
  }
  if (done) {
    aura(cx, cy, 200, YEL, 0.6 * (1 - prog(u, 0.935, 1.3)));
    flares(u - 0.935, cx, cy, YEL, 160);
    blit(T_REFL, cx, 700, { c: YEL, ax: 0.5, a: prog(u, 0.935, 1.0) });
  }
}

// Rewrite, u seconds in: by the equation e : a = b, the goal P(a) becomes
// P(b): an arrow falls from e toward the a, the a lifts out, the b drops
// in.
function rewrite(u: number): void {
  const w = (id: number) => SPRITES[id].w / S, sw = smooth(prog(u, 0.08, 0.24)), y = 560;
  const x0 = 720 - (w(M_RW[1]) + w(M_RW[2]) + w(M_RW[4])) / 2, xa = x0 + w(M_RW[1]);
  blit(M_RW[0], 720, 260, { c: GRNL, ax: 0.5, ay: 0.5 });
  const top: Pt = [720, 320], end: Pt = [xa + w(M_RW[2]) / 2, 430], f = prog(u, 0, 0.1);
  poly([top, end], 2.5, GRNL, 0.8 * (1 - prog(u, 0.22, 0.32)), f);
  if (f > 0 && f < 1) spark(...along([top, end], f));
  blit(M_RW[1], x0, y, { c: CREAM, ay: 0.5 });
  blit(M_RW[2], xa, y - 70 * sw, { c: CREAM, ay: 0.5, a: 1 - sw });
  blit(M_RW[3], xa, y + 70 * (1 - sw), { c: GRNL, ay: 0.5, a: sw });
  blit(M_RW[4], xa + lerp(w(M_RW[2]), w(M_RW[3]), sw), y, { c: CREAM, ay: 0.5 });
  aura(xa + w(M_RW[3]) / 2, y, 110, GRNL, 0.6 * prog(u, 0.2, 0.26) * (1 - prog(u, 0.26, 0.45)));
  flares(u - 0.22, xa + w(M_RW[3]) / 2, y, GRNL, 120);
}

// Absurd, u seconds in, from its hit: P and ¬P slam together in a red
// flash and burst; the contradiction collapses into ⊥.
function absurd(u: number): void {
  const cx = 720, cy = 450, red = hex(0xff453a), p = clamp(u / 0.45);
  if (u < 3 / 24) {
    const s = u / (3 / 24);
    blit(M_ABS[0], cx - 60 - 160 * (1 - s), cy, { c: CREAM, ax: 0.5, ay: 0.5, a: 1 - s });
    blit(M_ABS[1], cx + 60 + 160 * (1 - s), cy, { c: CREAM, ax: 0.5, ay: 0.5, a: 1 - s });
  }
  if (u < 2 / 24) fill(red, 0.3);
  if (p < 1) {
    aura(cx, cy, 80 + 200 * p, red, 0.9 * (1 - p));
    for (let k = 0; k < 12; ++k) {
      const a = (k + 0.5) * Math.PI / 6, r0 = 30 + 200 * p, r1 = 60 + 330 * (1 - (1 - p) ** 3);
      line(cx + r0 * Math.cos(a), cy + r0 * Math.sin(a), cx + r1 * Math.cos(a), cy + r1 * Math.sin(a), 3, red, 1 - p);
    }
  }
  // ⊥, in two strokes.
  poly([[cx, cy - 85], [cx, cy + 85]], 8, YEL, 1, prog(u, 0.05, 0.15));
  poly([[cx - 110, cy + 85], [cx + 110, cy + 85]], 8, YEL, 1, prog(u, 0.12, 0.22));
  aura(cx, cy, 160, YEL, 0.35 * prog(u, 0.1, 0.3));
}

// par figures

// NERV's orange: a core at work; white gold: a task burning.
const NERV = hex(0xff8a1a);
const HOT_W = hex(0xfff4c8);

// A pointy-top hexagon of radius r: its rim, drawn up to the fraction f.
function hexagon(x: number, y: number, r: number, w: number, c: Col, a = 1, f = 1): void {
  poly([0, 1, 2, 3, 4, 5, 6].map(k => [x + r * Math.cos(Math.PI / 6 + k * Math.PI / 3), y + r * Math.sin(Math.PI / 6 + k * Math.PI / 3)] as Pt), w, c, a, f);
}

// A filled pointy-top hexagon, antialiased by its distance to each pixel.
function hexfill(x: number, y: number, r: number, c: Col, a = 1): void {
  if (a <= 0) return;
  x *= S; y *= S; r *= S;
  const ap = r * Math.sqrt(3) / 2;
  const bx = Math.max(0, Math.floor(x - ap - 1)), ex = Math.min(W - 1, Math.ceil(x + ap + 1));
  const by = Math.max(0, Math.floor(y - r - 1)), ey = Math.min(H - 1, Math.ceil(y + r + 1));
  for (let py = by; py <= ey; ++py) {
    for (let px = bx; px <= ex; ++px) {
      const dx = Math.abs(px + 0.5 - x), dy = Math.abs(py + 0.5 - y);
      const cv = ap + 0.5 - Math.max(dx, 0.5 * dx + 0.866 * dy);
      if (cv > 0) paint(py * W + px, c, a * Math.min(1, cv));
    }
  }
}

// The cores: a honeycomb of radius 62 about the middle of the picture; each
// core's rank in [0, 1) is its turn to light, outward (its distance, plus
// some noise).
const HIVE: [number, number, number][] = (() => {
  const cs: [number, number, number][] = [];
  for (let j = -7; j <= 7; ++j) for (let i = -8; i <= 8; ++i) {
    const x = 720 + (i + (j & 1) / 2) * 62 * Math.sqrt(3), y = 540 + j * 93;
    cs.push([x, y, Math.hypot(x - 720, y - 540) + (i || j ? 260 * hash(cs.length, 120) : 0)]);
  }
  const ks = cs.map(c => c[2]).sort((a, b) => a - b);
  return cs.map(([x, y, d]) => [x, y, ks.indexOf(d) / ks.length]);
})();

type HiveOpt = { pen?: number; lit?: number; one?: number; z?: number };

// pen draws the rims, outward; lit is the time since the cores began to
// light, outward in 0.4 s; one, the time since the middle core lit alone.
// A core lights white and burns down to orange. z zooms into the middle.
function hive(o: HiveOpt): void {
  const z = o.z ?? 1, pen = o.pen ?? 1, dive = 1 - 1 / z;
  for (const [hx, hy, rank] of HIVE) {
    const x = 720 + (hx - 720) * z, y = 540 + (hy - 540) * z, r = 56 * z;
    if (x < -r || x > 1440 + r || y < -r || y > 1080 + r) continue;
    const e = rank === 0 && o.one !== undefined ? o.one : (o.lit ?? -1) - 0.4 * rank;
    if (e < 0) { hexagon(x, y, r, 2 * z, NERV, 0.35, prog(pen, rank * 0.7, rank * 0.7 + 0.3)); continue; }
    // The lone core flares.
    if (rank === 0 && o.one !== undefined) for (let i = 0; i < 3; ++i) { const p = prog(e, i * 0.07, i * 0.07 + 0.5); if (p > 0 && p < 1) ring(x, y, r + 160 * z * (1 - (1 - p) ** 3), 3, NERV, 1 - p); }
    const k = 1 - clamp(e / 0.1), q = r - 6 * z * (1 - k);
    const mix: Col = [lerp(NERV[0], 1, k), lerp(NERV[1], 1, k), lerp(NERV[2], 1, k)];
    hexfill(x, y, q, mix, (0.62 + 0.38 * k) * (1 - dive));
    hexagon(x, y, q, 3 * z, mix, 1);
    hexagon(x, y, 30 * z, 2.5 * z, BLACK, 0.5 * (1 - dive));
  }
}

// The recap under badge i: the film's own figure for its word, in dim line
// art drawn by the pen f: an active pair (affine), the game's room and flag
// (dependent), the induction chain (total), the sort's comparators (parallel).
function echo(i: number, f: number, c: Col, a: number): void {
  if (i === 0) {
    for (const d of [1, -1]) {
      const cx = 720 - d * 380, ax = cx + d * 110;
      tri_o(cx, 444, 110, d * Math.PI / 2, 3, c, a, f);
      for (const s of [1, -1]) poly(bez([cx - d * 55, 444 + s * 95], [cx - d * 55, 444 + s * 260], [cx - d * 200, 444 + s * 200], [cx - d * 300, 444 + s * 330]), 2, c, a, f);
      poly([[ax, 444], [720, 444]], 3, c, a, f);
    }
  } else if (i === 1) {
    for (let k = 0; k <= 12; ++k) line(180 + k * 88, 200, 180 + k * 88, lerp(200, 904, f), 1.2, c, a * 0.6);
    for (let k = 0; k <= 8; ++k) line(180, 200 + k * 88, lerp(180, 1236, f), 200 + k * 88, 1.2, c, a * 0.6);
    for (const [x, y] of [[3, 0], [3, 1], [3, 2], [3, 3], [0, 3], [1, 3], [2, 3]]) {
      const cx = 224 + x * 88, cy = 244 + y * 88;
      poly([[cx - 38, cy - 38], [cx + 38, cy - 38], [cx + 38, cy + 38], [cx - 38, cy + 38], [cx - 38, cy - 38]], 2.5, c, a, f);
      line(cx - 38, cy - 38, cx + 38, cy + 38, 1.5, c, a * f);
      line(cx + 38, cy - 38, cx - 38, cy + 38, 1.5, c, a * f);
    }
    poly([[298, 362], [298, 300], [338, 312], [298, 324]], 3, c, a, f);
  } else if (i === 2) {
    for (let k = 0; k < 8; ++k) {
      const x = 160 + k * 160, on = clamp(f * 9 - k) > 0.5;
      ring(x, 900, 36, 2.5, c, a, f);
      if (on) disc(x, 900, 24, c, a * 0.8);
      if (on) blit(T_DIG[k], x, 900, { c: BG, ax: 0.5, ay: 0.5, z: 0.55 });
      if (k < 7) { poly([[x + 44, 900], [x + 116, 900]], 2.5, c, a, f); poly([[x + 104, 890], [x + 116, 900], [x + 104, 910]], 2.5, c, a, f); }
    }
  } else {
    bitonic({ tree: f, wires: f, stage: 10 * f, c, hot: c, a });
  }
}

// The GPU's fork over the square: split l halves every task of split l - 1
// (the odd multiples of 1080 / 2^l); the newest split burns, older ones dim.
function splits(f: number, a: number): void {
  for (let l = 1; l <= Math.min(f, 6); ++l) {
    const n = 2 ** l, w = l === f ? 2.5 : 1.5, al = a * (l === f ? 1 : 0.25 + 0.08 * l);
    for (let k = 1; k < n; k += 2) {
      line(MX + 1080 * k / n, 0, MX + 1080 * k / n, 1080, w, GRNL, al);
      line(MX, 1080 * k / n, 1440, 1080 * k / n, w, GRNL, al);
    }
  }
}

// The menu's readout (the clock, the zoom): s glyph by glyph on a fixed
// pitch, its baseline at y.
function readout(s: string, y: number, c: Col): void {
  [...s].forEach((g, k) => { if (g !== " ") blit(T_DIG["0123456789.s".indexOf(g)], 50 + k * 40 + 20, y, { c, ax: 0.5, ay: 1 }); });
}

// net figures

// The two rules of interaction combinators. A pair of nodes is drafted edge
// by edge, led in by wires from four free ends, and slides in until the
// apexes (the principal ports) touch: a burst. Of one kind (two cons), they
// annihilate: both shrink into the contact, and the aux wires join straight
// across. Of two kinds (con and dup), they commute: each passes through the
// other, and the four copies close a square of wires. The choreography is
// written in its own seconds (s), 5.6 in all; rules() warps it to the film.
const R_R = 100, R_Y = 500;
const R_END: Pt[] = [[180, 320], [180, 680], [1260, 320], [1260, 680]];
const R_CON = { cel: GREEN, edge: CREAM }, R_DUP = { cel: GOLD, edge: GOLD };
const out3 = (x: number) => 1 - (1 - clamp(x)) ** 3;

// A node's corners around (x, y), the apex toward angle g (0 = right).
function r_tri(x: number, y: number, g: number, r: number): Pt[] {
  const ux = Math.cos(g), uy = Math.sin(g);
  return [[x + r * ux, y + r * uy], [x - 0.5 * r * ux - 0.866 * r * uy, y - 0.5 * r * uy + 0.866 * r * ux],
          [x - 0.5 * r * ux + 0.866 * r * uy, y - 0.5 * r * uy - 0.866 * r * ux]];
}

// Port q of a node (0 the apex, 1 and 2 the base; m = -1 mirrors): its place
// and the way its wire leaves, [x, y, tx, ty].
function r_port(x: number, y: number, g: number, q: number, m = 1): [number, number, number, number] {
  const ux = Math.cos(g), uy = Math.sin(g);
  if (q === 0) return [x + R_R * ux, y + R_R * uy, ux, uy];
  const s = q === 1 ? m : -m, tx = -ux - 0.35 * s * uy, ty = -uy + 0.35 * s * ux, l = Math.hypot(tx, ty);
  return [x - 0.5 * R_R * ux - 0.55 * R_R * s * uy, y - 0.5 * R_R * uy + 0.55 * R_R * s * ux, tx / l, ty / l];
}

// A node drafted over pd (its edges one by one, then its cel), scaled by z.
function r_node(x: number, y: number, g: number, k: { cel: Col; edge: Col }, pd = 1, a = 1, z = 1): void {
  const v = r_tri(x, y, g, R_R * z);
  for (let j = 0; j < 3; ++j) poly([v[j], v[(j + 1) % 3]], 3, k.edge, a, clamp(pd * 4 - j));
  tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], k.cel, 0.45 * a * clamp((pd - 0.75) * 4));
  disc(v[0][0], v[0][1], 6, CREAM, a);
}

// The wire from the free end e to port p, drawn from e over pd.
function r_lead(e: Pt, p: number[], pd = 1, a = 1): void {
  const d = Math.abs(p[0] - e[0]), sx = e[0] < 720 ? 1 : -1;
  poly(bez(e, [e[0] + sx * 0.5 * d, e[1]], [p[0] + 0.5 * d * p[2], p[1] + 0.5 * d * p[3]], [p[0], p[1]], 20), 3, GOLD, a, pd);
}

// A compass circle about (x, y) from angle g0, swept at s0, faded out at s1.
function r_swing(s: number, x: number, y: number, r: number, s0: number, s1: number, g0: number): void {
  const a = 0.55 * (1 - out3((s - s1) / 0.35)), f = clamp((s - s0) / 0.45);
  if (a <= 0 || f <= 0) return;
  poly(Array.from({ length: 49 }, (_, i) => [x + r * Math.cos(g0 + f * i * Math.PI / 24), y + r * Math.sin(g0 + f * i * Math.PI / 24)] as Pt), 2, CREAM, a);
}

// Two nodes led in from the free ends, sliding until they touch at s0 + 1.3;
// then the white contact line between their apexes.
function r_pair(s: number, s0: number, b: { cel: Col; edge: Col }): void {
  const sl = smooth((s - s0 - 0.9) / 0.4), xa = lerp(470, 720 - R_R, sl), xb = lerp(970, 720 + R_R, sl);
  r_swing(s, 470, R_Y, 130, s0, s0 + 0.9, Math.PI);
  r_swing(s, 970, R_Y, 130, s0 + 0.08, s0 + 0.95, 0);
  for (let j = 0; j < 2; ++j) {
    r_lead(R_END[j], r_port(xa, R_Y, 0, j + 1, -1), clamp((s - s0 - 0.35 - 0.08 * j) / 0.45));
    r_lead(R_END[j + 2], r_port(xb, R_Y, Math.PI, j + 1), clamp((s - s0 - 0.4 - 0.08 * j) / 0.45));
  }
  r_node(xa, R_Y, 0, R_CON, clamp((s - s0 - 0.05) / 0.6));
  r_node(xb, R_Y, Math.PI, b, clamp((s - s0 - 0.15) / 0.6));
  if (s > s0 + 0.95) line(xa + R_R, R_Y, xb - R_R, R_Y, 3, WHITE, clamp((s - s0 - 0.95) / 0.35));
}

// The free ends: a dot in a ring each, one after another from s0.
function r_ends(s: number, s0: number, a: number): void {
  R_END.forEach(([x, y], j) => {
    const q = out3((s - s0 - 0.06 * j) / 0.2) * a;
    disc(x, y, 7, CREAM, q);
    ring(x, y, 16, 2, CREAM, 0.5 * q);
  });
}

// A soft white glow.
function r_glow(x: number, y: number, r: number, a: number): void {
  for (let i = 1; i <= 6; ++i) disc(x, y, r * i / 6, WHITE, a * 0.25);
}

// The burst of a contact at s0: a flash of the frame (two frames long, film
// time f since the contact), a glow, a ring.
function r_burst(s: number, s0: number, f: number): void {
  const d = s - s0;
  if (d < 0 || d >= 0.5) return;
  r_glow(720, R_Y, 150, clamp(1 - d / 0.22));
  const q = out3(d / 0.5);
  ring(720, R_Y, 20 + 220 * q, 2 + 3 * (1 - q), WHITE, 1 - q);
  if (f < 2 / FPS) fill(WHITE, 0.2);
}

// A piecewise linear map through the knots [x, y], extended by its ends.
function warp(x: number, ks: Pt[]): number {
  let i = 0;
  while (i + 2 < ks.length && x > ks[i + 1][0]) ++i;
  const [[x0, y0], [x1, y1]] = [ks[i], ks[i + 1]];
  return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
}

// The rules at film time u from the start of the slot: each contact on a
// beat (u = 0.935, 1.869), about twice the pace of the choreography.
const R_ANN: Pt[] = [[0, 0], [0.935, 1.3], [1.535, 2.8]];
const R_COM: Pt[] = [[1.349, 2.65], [1.869, 3.95], [2.804, 5.6]];
function rules(u: number): void {
  // Annihilation: 0 to 2.8 s.
  const s = warp(u, R_ANN);
  if (s < 2.9) {
    const a = 1 - out3((s - 2.5) / 0.3);
    r_ends(s, 0.35, a);
    if (s < 1.3) r_pair(s, 0, R_CON);
    else {
      // They vanish into the contact; the wires join and straighten.
      const q = smooth((s - 1.35) / 0.8), v = 1 - out3((s - 1.3) / 0.3);
      for (let j = 0; j < 2; ++j) {
        const e0 = R_END[j], e1 = R_END[j + 2], p = r_port(720 - R_R, R_Y, 0, j + 1, -1), b = r_port(720 + R_R, R_Y, Math.PI, j + 1);
        const c: Col = [0, 1, 2].map(i => lerp(WHITE[i], GOLD[i], q)) as Col;
        poly(bez(e0, [lerp(p[0], 540, q), lerp(p[1], e0[1], q)], [lerp(b[0], 900, q), lerp(b[1], e1[1], q)], e1), 3, c, a);
      }
      if (v > 0) for (const [x, g] of [[720 - R_R, 0], [720 + R_R, Math.PI]]) {
        r_node(720 + (x - 720) * (0.3 + 0.7 * v), R_Y, g, R_CON, 1, v, 0.3 + 0.7 * v);
      }
      // The joined wires light once, end to end.
      const g = clamp((s - 2.05) / 0.35);
      if (g > 0 && g < 1) for (let j = 0; j < 2; ++j) r_glow(lerp(180, 1260, g), R_END[j][1], 26, 0.8 * (1 - g));
    }
    r_burst(s, 1.3, u - R_ANN[1][0]);
  }
  // Commutation: 2.65 to 5.6 s.
  const t = warp(u, R_COM);
  if (t >= 2.65) {
    r_ends(t, 2.85, 1);
    if (t < 3.95) r_pair(t, 2.65, R_DUP);
    else {
      // Each passes through the other: two dups go left, two cons right.
      const q = smooth((t - 3.95) / 0.95), dy = 180 * q, bx = lerp(720 + R_R, 500, q), ax = lerp(720 - R_R, 940, q);
      const B: Pt[] = [[bx, R_Y - dy], [bx, R_Y + dy]], C: Pt[] = [[ax, R_Y - dy], [ax, R_Y + dy]];
      const bp = (i: number, k: number) => r_port(B[i][0], B[i][1], Math.PI, k);
      const cp = (i: number, k: number) => r_port(C[i][0], C[i][1], 0, k, -1);
      // The square's cel, as it completes.
      const f = 0.24 * out3((t - 4.8) / 0.4);
      // The square: dup i's aux k meets con k's aux i.
      for (let i = 0; i < 2; ++i) for (let k = 0; k < 2; ++k) {
        const p = bp(i, k + 1), c = cp(k, i + 1);
        line(p[0], p[1], c[0], c[1], 3, GOLD);
      }
      for (let i = 0; i < 2; ++i) { r_lead(R_END[i], bp(i, 0)); r_lead(R_END[i + 2], r_port(C[i][0], C[i][1], 0, 0)); }
      const sq = [bp(0, 1), cp(0, 1), cp(1, 2), bp(1, 2)];
      tri(sq[0][0], sq[0][1], sq[1][0], sq[1][1], sq[2][0], sq[2][1], GREEN, f);
      tri(sq[0][0], sq[0][1], sq[2][0], sq[2][1], sq[3][0], sq[3][1], GREEN, f);
      r_swing(t, 720, R_Y, 300, 4.75, 9, -Math.PI / 2);
      for (let i = 0; i < 2; ++i) { r_node(B[i][0], B[i][1], Math.PI, R_DUP); r_node(C[i][0], C[i][1], 0, R_CON); }
    }
    r_burst(t, 3.95, u - R_COM[1][0]);
  }
  blit(t < 2.65 ? T_RULE["dup-dup-annihilate"] : T_RULE["dup-dup-commute"], 64, 56, { c: TXT });
}

// cards figures

// A card in a heavy Didone (Bodoni 72 bold, near NGE's Matisse), pressed to
// 86% of its width: the lines stacked and centered, the widest fit to fit px,
// the block at most 700 px tall.
function press(lines: string[], fit = 1150): number {
  const l = (w: string) => `align(center, text(font: "Bodoni 72", weight: "bold", size: 300pt, ${str(w)}))`;
  return sprite(`#context {
  let b = stack(dir: ttb, spacing: 80pt, ${lines.map(l).join(", ")})
  let m = measure(b)
  let z = calc.min(${fit}pt / (0.86 * m.width), 700pt / m.height)
  scale(x: z * 86%, y: z * 100%, reflow: true, b)
}`);
}

// A chorus card: the word on black (on white if inv), pushed in slowly over
// its beat; left sets it at x 147, as NGE sets its small-caps cards.
function hit(id: number, u: number, o: { inv?: boolean; left?: boolean } = {}): void {
  fill(o.inv ? WHITE : BLACK);
  blit(id, o.left ? 147 : 720, 540, { c: o.inv ? BLACK : WHITE, ax: o.left ? 0 : 0.5, ay: 0.5, z: 1 + 0.05 * u / 0.4674 });
}

// LAWS.bend on the CRT: a sheet with its file name on a tab, then the law,
// typed a line at a time (the body indented two columns), a block cursor at
// the pen.
function laws(u: number): void {
  const x = 140, y = 380, pitch = 103, adv = 27.6;
  const pen = at(u, 0, 0.12);
  const tab = 150 + SPRITES[T_LAWS[0]].w / S;
  poly([[tab, 250], [1350, 250], [1350, 830], [90, 830], [90, 250], [110, 250]], 2, LINE, 0.55, pen);
  blit(T_LAWS[0], 130, 250, { c: DIM, ay: 0.5, a: pen });
  for (let i = 0; i < 4; ++i) {
    const k = prog(u, 0.08 + 0.13 * i, 0.21 + 0.13 * i), id = T_LAWS[i + 1];
    if (k <= 0) break;
    const lx = x + (i ? 2 * adv : 0), ly = y + i * pitch, w = SPRITES[id].w / S;
    blit(id, lx, ly, { c: i ? TXT : YEL, ay: 0.5, show: k });
    const last = i === 3 || u < 0.21 + 0.13 * i;
    if (last && (k < 1 || Math.floor(u * 8) % 2 === 0)) rect(lx + w * k + 6, ly - 24, 24, 48, TXT, 0.8);
  }
}

// Scenes
// ======

const at = (u: number, a: number, b: number) => smooth(prog(u, a, b));

// Captions (x, y are the top-left of the first line; pitch is the line
// step); each line appears whole at its time.
function caps(ids: number[], x: number, y: number, pitch: number, when: number[], u: number, c: Col = TXT, a = 1): void {
  ids.forEach((id, i) => { if (u >= when[i]) blit(id, x, y + i * pitch, { c, a }); });
}

// shared the opening, the sort, the LAW card: nobody edits these lines.
const S_KIKAKU  = mincho("企画・原作", 50);
const S_HOC     = serif("HIGHER ORDER CO.", 66, 0.02);
const S_PROJ    = [mincho("企画", 52), serif("Project Bend.", 76), mincho("掲載", 52), serif("LAWS.bend", 70), serif("PROOF.bend", 70)];
const T_RULE: Record<string, number> = {};
{
  const b = mono("β-reduction", 44, 0.3), c = mono("commutation", 44, 0.3), a = mono("annihilation", 44, 0.3);
  Object.assign(T_RULE, { "beta": b, "dup-lam": c, "dup-app": c, "dup-dup-commute": c, "dup-dup-annihilate": a });
}
const T_FORKJ   = mono("Fork-Join-Fork", 60);
const T_BITONIC = mono("bitonic sort", 52, 0.45);
const T_DEPTH   = mono("depth O(log² n)", 52, 0.2);
const C_LAW     = flash("LAW", 440, "", 900);

// title sprites
const S_BEND    = serif("BEND", 250, 0.14);
// The Japanese of the title in a sharp Mincho (Hiragino W6), no stroke.
const S_KATA    = sprite(`#skew(ax: -12deg, reflow: true, scale(x: 120%, reflow: true, text(font: "Hiragino Mincho ProN", weight: "bold", size: 300pt, "ベンド")))`);
const S_SHIN    = sprite(`#text(font: "Hiragino Mincho ProN", weight: "bold", size: 100pt, "並列証明")`);

// seq sprites
const Q_SEQ     = [tex("⊢ S(p+0) = S(p+0)"), tex("p+0 = p#h(0.3em) ⊢ S(p+0) = S(p)"), tex("p+0 = p#h(0.3em) ⊢ S(p)+0 = S(p)"), tex("⊢ 0+0 = 0"), tex("⊢ ∀n.#h(0.35em) n+0 = n", 72)];
const Q_IH      = tex("p+0 = p");
const Q_RULE    = ["refl", "=L", "conv", "ind"].map(s => mono(s, 36, 0.05));

// gpu sprites
const T_CORES   = mono("4,096 cores", 52, 0.3);
const T_POW2    = mono("pow2!(20n)", 40, 0.1);

// logo sprites

// board sprites
const T_LAW     = mono("LAW: the flag is unreachable", 54, 0.06);
const T_LAWW    = mono("LAW:", 54, 0.06);
const T_MOVES   = mono("∀ moves. ¬won", 40, 0.06);
const T_CEX     = mono("counterexample", 36, 0.1);
// A red rubber stamp, askew.
const C_STAMP   = sprite(`#rotate(-12deg, reflow: true, box(stroke: 8pt + white, radius: 8pt, inset: (x: 30pt, y: 18pt), text(font: "Superclarendon", weight: "bold", size: 96pt, "EXPLOIT")))`);

// proof sprites
const T_ASK     = [mono("Which laws must", 64), mono("always hold", 64), mono("in order to trust", 64), mono("code no one reads?", 64)];
const T_NAME    = ["induction", "case analysis", "reflexivity", "rewrite", "absurd"].map(w => mono(w, 72, 0.25));
const T_CASE    = [mono("case 0n", 50, 0.1), mono("case 1n+p", 50, 0.1)];
const T_REFL    = mono("{==}", 84, 0.1);
const M_NUM     = ["0", "1", "2", "3", "4", "5", "⋯", "∞"].map(s => math(s, s === "⋯" ? 90 : 52));
const M_GOAL    = math("n", 150);
const M_CASE    = [math("0", 130), math("1 + n", 130)];
const M_ABS     = [math("P", 130), math("¬P", 130)];
const M_RW      = [math("e : a = b", 100), math("P(", 180), math("a", 180), math("b", 180), math(")", 180)];

// par sprites
const T_BADGE   = ["1", "2", "3", "4"].map(d => clar(d, 150));
const T_BWORD   = ["affine", "dependent", "total", "parallel"].map(w => mono(w, 84, 0.3));
const T_LANG    = ["C", "Bend"].map(w => mono(w, 120, 0));
const T_DIG     = [..."0123456789.s"].map(g => mono(g, 56, 0));
const C_66      = flash("66×", 420, "", 900);
const C_PAR     = flash("PARALLEL", 300);
const C_EVERY   = flash("EVERY CORE", 300);

// net sprites
const C_OBR     = smallcaps("OPTIMAL", "BETA", "REDUCTION");
// The term each state of the net denotes : a round
// of βs on shared redexes reduces every copy at once.
const T_LAMBDA  = ["(λf.λx.f (f x)) (λf.λx.f (f x))", "λf.(λg.λx.g (g x)) ((λg.λx.g (g x)) f)",
  "λf.λx.(λy.f (f y)) ((λy.f (f y)) x)", "λf.λx.f (f (f (f x)))"].map(s => mono(s, 36, 0.08));
const TERM_AT   = [0, 1, 1, 2, 2, 3];

// cards sprites
const C_PROOF   = flash("PROOF", 400);
const C_DTT     = smallcaps("DEPENDENT", "TYPE", "THEORY");
const C_PAT     = plain("PROPOSITIONS AS TYPES", 200);
const C_SREC    = press(["STRUCTURAL", "RECURSION"]);
const C_MPE     = smallcaps("MASSIVELY", "PARALLEL", "EXECUTION");
const C_BIDI    = press(["BIDIRECTIONAL", "CHECKING"]);
const C_LINEAR  = plain("LINEAR LOGIC", 220);
const C_CONFL   = flash("CONFLUENCE", 280);
const C_CONSIS  = plain("CONSISTENCY", 220);
const C_TEST    = press(["DON'T", "TEST IT"]);
const C_PROVE   = plain("PROVE IT", 170, 1150);
const C_BEND    = plain("BEND", 300);
const T_LAWS    = [mono("LAWS.bend", 40, 0.3), ...["law you_cant_win:", "for moves: List<Game.Move>", "board = Game.replay(Game.start(), moves)", "{Game.is_won(board) == False{} : Bool}"].map(s => mono(s, 46, 0))];
const C_FORALL  = math("∀x. P(x)", 260);
const C_STAFF   = [mincho("言語", 170, "ttb"), mincho("ベンド", 300)];
const T_END     = [mono("produced by", 30, 0.2), mono("Higher Order Co.", 44, 0.18), mono("for", 30, 0.2), mono("bend-lang.com", 44, 0.18)];

type Cut = { t: number; draw: (u: number, t: number) => void; card?: boolean };

// A flash card: white on black, clean (no tube).
const card = (id: number): ((u: number) => void) => () => { fill(BLACK); blit(id, 720, 540, { c: WHITE, ax: 0.5, ay: 0.5 }); };

const CUTS: Cut[] = [
  // ==== title 0-1.84: the drop ====
  // The opening, on NGE's own: a drop in the dark and its ring, each with
  // a faint wide ring of light under it.
  { t: 0, card: true, draw: u => {
    fill(BLACK);
    const a = at(u, 0.1, 0.5) * (1 - at(u, 0.85, 1.1));
    disc(720, 540, 16, RING, 0.2 * a);
    disc(720, 540, 5, RING, a);
    for (const d of [0, 0.25]) {
      if (u <= 0.85 + d) continue;
      const r = 560 * smooth(prog(u, 0.85 + d, 1.84)), f = 1 - prog(u, 1.4, 1.84);
      ring(720, 540, r, 14, RING, 0.15 * f);
      ring(720, 540, r, 2, RING, f);
    }
  } },
  // ==== logo 1.84-6.5: the red sea ====
  // The red sea: the credit, and the mark of HOC in thin gold, drawn in,
  // then one slow turn of its reduction loop, moving away.
  { t: 1.84, draw: u => {
    clouds(u * 0.3, hex(0x2a0402), hex(0xff2a0a), 1, 340, 21);
    clouds(u * 0.2, hex(0x100000), hex(0xa01006), 0.35, 110, 23);
    stars(u, 120, 30, 0.5);
    const z = lerp(1250, 330, smooth(prog(u, 1.3, 4.66)));
    const al = at(u, 0.5, 1.0) * (1 - at(u, 4.3, 4.66)), pen = at(u, 0.5, 1.2), w = Math.max(2, 3.2 * z / 1000);
    logo({ z, c: GOLD, fc: hex(0xff9030), fill: 0.3 * pen, w, glow: 3 * w, a: al, nodes: pen, pen, loop: Math.max(0, u - 1.2) / 3.2 });
    blit(S_KIKAKU, 720, 420, { c: WHITE, ax: 0.5, a: at(u, 0.6, 0.9) * (1 - at(u, 4.0, 4.5)) });
    blit(S_HOC, 720, 500, { c: WHITE, ax: 0.5, a: at(u, 0.6, 0.9) * (1 - at(u, 4.0, 4.5)) });
    lids(prog(u, 0, 1.2));
  } },
  // ==== net 6.5-13.68: the blue sea, the net of 2 2 ====
  // Red turns blue under the rays; the net of 2 2 opens like a tree. The
  // red sea goes on where it left off (T, its clock) as the blue comes in.
  { t: 6.5, draw: u => {
    const b = at(u, 0, 1.2), T = u + 4.66;
    clouds(T * 0.3, hex(0x2a0402), hex(0xff2a0a), 1 - b, 340, 21);
    clouds(T * 0.2, hex(0x100000), hex(0xa01006), 0.35 * (1 - b), 110, 23);
    stars(T, 120, 30, 0.5 * (1 - b));
    clouds(u * 0.2, hex(0x020a30), hex(0x2a60e0), b, 300, 25);
    streaks(u, 170, b);
    rays(720, -300, 260, 200, 1400, hex(0xc8d8ff), 0.35 * b * (1 - at(u, 3.0, 3.5)), 100, 0.3, Math.PI - 0.3);
    // Five parallel rounds, two beats apart: each round's pairs flare
    // together on a beat (7.371 + 0.935 r, the verse grid run backward).
    const k = clamp((u - 1.432) / 0.935, 0, 5), sky = hex(0xe0ecff), blob = at(u, 4.3, 4.9);
    net(k, { c: sky, hot: WHITE, dup: hex(0x6f9cff), t: u, draw: at(u, 0, 1.0), zoom: lerp(1.05, 0.85, smooth(prog(u, 0, 6.8))), cy: 490, a: 1 - 0.45 * blob, calm: blob });
    // The credits over a glowing blob, the net calm and dim behind; a soft
    // dark glow around each word.
    if (blob > 0) {
      clouds(u * 0.5, hex(0x0a1a60), hex(0x90c8ff), 0.2 * blob, 160, 27);
      const a = blob * (1 - at(u, 6.6, 6.85));
      for (const [i, x, y] of [[0, 300, 380], [1, 470, 368], [2, 300, 560], [3, 470, 548], [4, 470, 668]]) {
        for (let l = 0; l < 8; ++l) blit(S_PROJ[i], x + 7 * Math.cos(l * Math.PI / 4), y + 7 * Math.sin(l * Math.PI / 4), { c: hex(0x061448), a: 0.1 * a });
        blit(S_PROJ[i], x, y, { c: WHITE, a });
      }
    }
    // The term follows the net: a new one is typed as its state settles.
    const j = Math.min(5, Math.floor(k + 0.3)), q = TERM_AT[j];
    blit(T_LAMBDA[q], 720, 1000, { c: sky, ax: 0.5, a: at(u, 1.0, 1.3) * (1 - 0.5 * blob) * (1 - at(u, 6.6, 6.85)), show: j && q !== TERM_AT[j - 1] ? (k + 0.3 - j) / 0.3 : 1 });
    // The normal form bursts: white rings.
    if (u > 6.95) { const q = prog(u, 6.95, 7.18); disc(720, 540, 160 * q, WHITE, 1 - q * 0.5); ring(720, 540, 900 * q, 10, WHITE, 1); ring(900, 700, 500 * q, 8, WHITE, 1); }
  } },
  // ==== title 13.68-22.16: the clouds, the name, the title ====
  { t: 13.68, card: true, draw: () => { fill(hex(0xedffff)); } },
  // The first hit: clouds, black nodes strobing through; the name comes out
  // of the clouds.
  { t: 13.78, draw: u => {
    clouds(u, hex(0x303434), hex(0xd8dcd8), 1, 260, 5);
    if (Math.floor(u * 24) % 2 === 0 && u < 1.0) {
      for (let i = 0; i < 5; ++i) {
        const x = 200 + i * 260 + 60 * Math.sin(u * 3 + i), y = 420 + 160 * Math.cos(i * 1.7);
        const r = 110 + 40 * hash(i, 5), g = i * 1.3 + u;
        const v: Pt[] = [0, 1, 2].map(k => [x + r * Math.sin(g + k * 2.094), y - r * Math.cos(g + k * 2.094)] as Pt);
        tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], BLACK, 0.9);
      }
    }
    blit(S_BEND, 720, 470, { c: hex(0x303030), ax: 0.5, ay: 0.5, a: at(u, 1.0, 1.5) });
  } },
  // The title in three parts: the name; the katakana, blue then burning
  // orange through white heat; the kanji over it; a ring; a cross of light.
  { t: 15.3, card: true, draw: u => {
    fill(BLACK);
    const kata = at(u, 1.55, 1.75), burn = at(u, 2.55, 2.8), heat = Math.sin(Math.PI * burn);
    bars(u, burn, kata);
    if (u > 5.05 && u < 5.8) {
      const p = prog(u, 5.05, 5.8), r = lerp(440, 600, 1 - (1 - p) ** 2), a = 1 - p;
      ring(720, 540, r + 16 + 30 * p, 1, hex(0xc8e0ff), 0.35 * a);
      gring(720, 540, r, 6, hex(0x3a78ff), a);
      gring(720, 540, r - 26 - 20 * p, 3, hex(0x3a78ff), 0.5 * a);
      gring(720, 540, r - 60 - 40 * p, 1.5, hex(0x3a78ff), 0.25 * a);
    }
    // The cross's warm bloom, behind the name.
    const x = at(u, 6.4, 6.8);
    if (x > 0) sdot(720, 470, 90 + 520 * x, hex(0xffd8a8), Math.min(0.8, 1.2 * x));
    const name = { c: hex(0xe8e8e8), ax: 0.5, ay: 0.5, a: 1 - 0.3 * kata };
    haze(S_BEND, 720, 470, 9, { ...name, c: hex(0xffe0c0) });
    blit(S_BEND, 720, 470, name);
    if (kata > 0) {
      const c: Col = [lerp(lerp(0.35, 0.93, burn), 1, heat), lerp(lerp(0.6, 0.36, burn), 0.85, heat), lerp(lerp(1, 0.1, burn), 0.55, heat)];
      blit(S_KATA, 760, 740, { c: BLACK, ax: 0.5, ay: 0.5, a: kata, z: 1.02 });
      haze(S_KATA, 760, 740, 12, { c, ax: 0.5, ay: 0.5, a: kata * (1 + heat) });
      blit(S_KATA, 760, 740, { c, ax: 0.5, ay: 0.5, a: kata });
    }
    const ka = at(u, 3.55, 3.7);
    haze(S_SHIN, 720, 300, 6, { c: hex(0xff6030), ax: 0.5, ay: 0.5, a: ka });
    blit(S_SHIN, 720, 300, { c: hex(0xe04a24), ax: 0.5, ay: 0.5, a: ka });
    // The cross of light: two sharp beams with a bloom, fine streaks, a
    // white-hot heart that fills the frame white on the cut.
    if (x > 0) {
      const cx = 720, cy = 540, L = 1000 * Math.min(1, x * 2.5);
      for (const g of [lerp(0.72, 0.66, x), lerp(-0.72, -0.66, x)]) {
        gline(cx - L * Math.cos(g), cy - L * Math.sin(g), cx + L * Math.cos(g), cy + L * Math.sin(g), 3 + 30 * x * x, hex(0xc8e0ff), 1);
      }
      for (let i = 0; i < 40; ++i) {
        const g = 2 * Math.PI * hash(i, 97), l = (150 + 750 * hash(i, 98)) * x;
        line(cx, cy, cx + l * Math.cos(g), cy + l * Math.sin(g), 0.6, hex(0xe8f0ff), 0.7 * x * hash(Math.floor(u * FPS), i + 99));
      }
      gdot(cx, cy, 6 + 40 * x * x, WHITE, 1);
      const q = at(u, 6.5, 6.8);
      if (q > 0) sdot(cx, cy, 80 + 700 * q, WHITE, Math.min(1, 2 * q));
      if (u >= 6.8) fill(WHITE);
    }
  } },
  // ==== board 22.16-37.5: the game ====
  { t: 22.16, card: true, draw: () => fill(WHITE) },
  // The verse: the board drafted calmly (frame, grid, compass, room, flag),
  // no text; the player comes, walks up to the room and tries to cross its
  // wall once: it bumps and recoils.
  { t: 22.328, draw: (u, t) => {
    const k = (t - gbeat(-23)) / HB, me = t < gbeat(-21.5) ? walk(P_COME, k) : lunge([4, 1], [-1, 0], t - gbeat(-21));
    game({ b: u / 2, me: t >= gbeat(-23.5) ? me : null, born: t - gbeat(-23.5), trail: [P_COME, k] });
    bump(t - gbeat(-21), GX + 4 * CELL - 6, GY + 6, GX + 4 * CELL - 6, GY + 4 * CELL - 6);
    fill(WHITE, 1 - prog(u, 0, 0.16));
  } },
  { t: 28.404, card: true, draw: u => hit(C_LAW, u) },
  // The law, typed; the board wraps (its copies tile around it); the player
  // leaves by the east edge, comes in by the west, reaches the flag: the
  // law broken, stamped EXPLOIT.
  { t: 28.872, draw: (u, t) => {
    const k = (t - gbeat(-17.5)) / HB;
    game({ me: walk(P_WRAP, k), wrap: at(u, 0.1, 0.35), won: t - gbeat(-13), trail: [P_WRAP, k] });
    law_cap(prog(u, 0.03, 0.5), t - gbeat(-13), -1);
  } },
  { t: 32.611, card: true, draw: u => hit(C_PROOF, u) },
  // The proof: the far walls are conjured, a pair per half beat; the player
  // bumps one; the law holds.
  { t: 33.078, draw: (_, t) => {
    const k = (t - gbeat(-4.5)) / HB, me = t < gbeat(-3.5) ? walk(P_FIX, k) : lunge([10, 1], [1, 0], t - gbeat(-3));
    game({ me, wrap: 1, fix: t - gbeat(-8), trail: [P_FIX, k] });
    bump(t - gbeat(-3), GX + 11 * CELL + 6, GY + 6, GX + 11 * CELL + 6, GY + 4 * CELL - 6);
    law_cap(1, 0, t - gbeat(-2));
  } },
  // ==== proof 37.5-52.09: the question, induction, the tactics ====
  // The verse's beats fall on 37.285 + 0.4674 j.
  // The question, line by line, over the whole picture; between its halves
  // a net pops in, a node per beat; one big node flies through.
  { t: 37.5, draw: u => {
    caps(T_ASK.slice(0, 2), 90, 100, 132, [0, 0.5], u);
    caps(T_ASK.slice(2), 90, 800, 132, [2.4, 2.9], u);
    ask_net(u, [0, 1, 2, 3, 4, 5].map(i => 1.187 + i * 0.4674), 3.991);
    if (u > 1.5 && u < 3.5) {
      const q = prog(u, 1.5, 3.5), x = lerp(-200, 1600, q), y = lerp(900, 150, q);
      const v = tri_o(x, y, 150, q * 4, 4, WHITE, 1);
      tri(v[0][0], v[0][1], v[1][0], v[1][1], v[2][0], v[2][1], WHITE, 0.15);
      for (const [px, py] of v) line(px, py, px + (px - x) * 0.45, py + (py - y) * 0.45, 4, WHITE, 1);
    }
  } },
  // Induction, at half speed: the chain draws, the base lights on a beat,
  // the light runs a node per beat to ∞; the word comes on the beat after.
  { t: 43.829, draw: u => {
    induct(u / 2);
    tac_name(T_NAME[0], u - 5.608, YEL);
  } },
  // The tactics, one per hit of the song (the frames at or just before its
  // onsets 50.34, 50.80, 51.27, 51.74), each caught near its end.
  { t: 50.333, draw: u => { cases(0.6 + 1.3 * u); tac_name(T_NAME[1], u, CYAN); } },
  { t: 50.791, draw: u => { refl(0.5 + 1.5 * u); tac_name(T_NAME[2], u, LAV); } },
  { t: 51.25, draw: u => { rewrite(u); tac_name(T_NAME[3], u, GRNL); } },
  { t: 51.708, draw: u => { absurd(u); tac_name(T_NAME[4], u, ORANGE, false); } },
  // ==== gpu 52.09-57.76: the bitonic sort ====
  // The sort: the fork tree grows over the wires (ochre); the stages fire
  // (blue).
  { t: 52.09, draw: u => {
    rect(0, 0, 1440, 1080, F_OCH, 0.35);
    bitonic({ tree: at(u, 0.1, 1.4), wires: at(u, 0.9, 2.0), stage: Math.max(0, (u - 2.0) / 0.42), c: OCHRE, hot: YEL });
    blit(T_FORKJ, 130, 50, { c: TXT, a: at(u, 0.2, 0.3) });
  } },
  { t: 55.7, draw: u => {
    rect(0, 0, 1440, 1080, F_BLU, 0.35);
    // The last stages speed up to finish by 57.32; on the verse beat 57.383
    // a line runs down the sorted keys.
    const sw = at(u, 1.683, 1.93);
    bitonic({ stage: Math.min(10, (u + 3.61 - 2.0) / 0.42 + 0.88 * u * u), sweep: sw, c: CYAN, hot: YEL });
    if (u > 1.2) blit(T_BITONIC, 130, 990, { c: CYAN });
    if (u >= 1.683) blit(T_DEPTH, 130, 50, { c: YEL });
  } },
  // ==== par 57.76-66.965: the badges, the bridge, one core, every core ====
  // The recap: four badges, one per beat, each over a dim echo of what the
  // film showed for its word.
  ...[57.76, 58.16, 58.59, 58.99].map((t, i) => ({ t, draw: (u: number) => {
    const bg = [hex(0x8a7443), hex(0x29451d), hex(0xd53843), hex(0x5a8adf)][i];
    const fg = [CREAM, CREAM, hex(0xffffad), WHITE][i];
    const fade = 1 - prog(u, 0.28, 0.4);
    echo(i, at(u, 0, 0.22), [OCHRE, GRNL, REDL, CYAN][i], 0.15 * fade);
    rect(615, 324, 210, 240, bg, fade);
    blit(T_BADGE[i], 720, 444, { c: fg, ax: 0.5, ay: 0.5, a: fade });
    haze(T_BWORD[i], 720, 680, 12, { c: BG, ax: 0.5, a: 12 * fade });
    blit(T_BWORD[i], 720, 680, { c: WHITE, ax: 0.5, a: fade });
  } })),
  // The bridge: the cores of a machine; one lights; the camera dives into it.
  { t: 59.53, draw: u => hive({ pen: at(u, 0.1, 1.8), one: u > 1.87 ? u - 1.87 : undefined, z: Math.exp(Math.log(20) * at(u, 2.9, 3.97) ** 2) }) },
  // One thread of C: the set, a row at a time, on the clock; its clock runs
  // at 0.815 speed, so the set is 55% done (of its 3.75 s) at the cut.
  { t: 63.5, draw: u => {
    const s = u * 0.815;
    mandel({ rows: s / 3.75, beam: (u * 7.3) % 1 });
    blit(T_LANG[0], 50, 60, { c: TXT });
    readout(s.toFixed(3) + " s", 306, TXT);
  } },
  // The chorus. The GPU, in frames: the square forks into 64 tasks, and
  // each draws its own rows, all at once (0-7); the clock stops at 0.057 s,
  // a shock rolls out, the set holds (7-12); the camera dives 1500x into a
  // seahorse's spiral (12 to the cut).
  { t: 66.03, draw: u => {
    const F = u * 24, q = prog(F, 7, 14), z = Math.exp(Math.log(1500) * (1 - (1 - prog(F, 12, 22.44)) ** 2));
    mandel({ k: 8, rows: prog(F, 0.5, 7), beam: (u * 7.3) % 1, z });
    if (F < 12) splits(3, 1 - prog(F, 7, 11));
    rect(MX, 0, 1080, 1080, WHITE, 0.3 * (1 - prog(F, 0, 3)));
    if (q > 0 && q < 1) { ring(SEA[0], SEA[1], 900 * (1 - (1 - q) ** 3), 12 * (1 - q) + 2, HOT_W, 1 - q); ring(SEA[0], SEA[1], 700 * (1 - (1 - q) ** 2), 3, WHITE, 0.6 * (1 - q)); }
    // The menu column, over the shock.
    rect(0, 0, MX, 1080, BG, 0.94);
    line(MX, 0, MX, 1080, 2, LINE, 0.22);
    blit(T_LANG[1], 50, 60, { c: TXT });
    readout((0.057 * prog(F, 0.5, 7)).toFixed(3) + " s", 306, F < 7 ? TXT : GRNL);
  } },
  // ==== chorus: one slot per beat group, b(k) = 66.03 + 0.4674 k ====
  // k2 par PARALLEL: the card cuts the dive
  { t: 66.965, card: true, draw: card(C_PAR) },
  // k3 par 66×
  { t: 67.432, card: true, draw: card(C_66) },
  // k4-5 par the hive: a honeycomb of cores lights outward
  { t: 67.900, draw: u => { fill(BLACK); hive({ lit: (u - 0.03) * 0.75 }); } },
  // k6 par EVERY CORE
  { t: 68.834, card: true, draw: card(C_EVERY) },
  // k7-12 net annihilation, then commutation
  { t: 69.302, draw: rules },
  // k13 net OPTIMAL BETA REDUCTION
  { t: 72.106, card: true, draw: () => { fill(BLACK); blit(C_OBR, 147, 540, { c: WHITE, ay: 0.5 }); } },
  // k14 cards LAW
  { t: 72.574, card: true, draw: u => hit(C_LAW, u) },
  // k15-16 cards LAWS.bend: the law, typed
  { t: 73.041, draw: laws },
  // k17-19 seq a proof in sequent calculus, drafted
  { t: 73.976, draw: sequent },
  // k20-24 cards titles and recalls, a beat or a half beat each
  { t: 75.378, card: true, draw: u => hit(C_DTT, u, { left: true }) },
  { t: 75.845, card: true, draw: u => hit(C_PAT, u, { inv: true }) },
  { t: 76.079, draw: u => induct(2.6 + u) },
  { t: 76.313, card: true, draw: u => hit(C_SREC, u) },
  { t: 76.546, draw: u => game({ me: [1, 1], wrap: 1, won: 0.25 + u }) },
  { t: 76.780, card: true, draw: u => hit(C_TEST, u) },
  { t: 77.248, card: true, draw: u => hit(C_PROVE, u, { inv: true }) },
  // k25-27 gpu pow2 on the GPU: the forks descend (b25-b26), the
  // cores light (b26-b27), the results join back up
  { t: 77.715, draw: u => {
    const B = 0.4674;
    split3d(12 * prog(u, 0, B) ** 0.8, Math.max(0, (u - B) / B), prog(u, 2 * B, 2.9 * B), u / (3 * B));
    if (u >= 2 * B) blit(T_CORES, 58, 60, { c: TXT });
  } },
  // k28 logo the mark, 2 frames: the subliminal flash
  { t: 79.117, card: true, draw: () => { fill(WHITE); logo({ z: 800, c: BLACK, fc: BLACK, w: 8 }); } },
  // k28 cards MASSIVELY PARALLEL EXECUTION
  { t: 79.2, card: true, draw: u => hit(C_MPE, u, { left: true }) },
  // k29-38 cards titles and recalls, then LAW PROOF BEND and the staff
  { t: 79.585, draw: u => mandel({ z: 300 * (1 + 4 * u) }) },
  { t: 79.818, draw: u => { fill(BLACK); hive({ lit: 0.6 + 3 * u }); } },
  { t: 80.052, card: true, draw: u => hit(C_BIDI, u) },
  { t: 80.519, draw: u => bitonic({ stage: 3 + 30 * u, c: OCHRE, hot: YEL }) },
  { t: 80.753, draw: u => rules(0.9 + u) },
  { t: 80.987, card: true, draw: u => hit(C_LINEAR, u, { inv: true }) },
  { t: 81.220, draw: u => cases(1.0 + u) },
  { t: 81.454, card: true, draw: u => hit(C_CONFL, u) },
  { t: 81.688, card: true, draw: u => hit(C_CONSIS, u, { inv: true }) },
  { t: 81.922, card: true, draw: u => hit(C_LAW, u) },
  { t: 82.389, card: true, draw: u => hit(C_PROOF, u) },
  { t: 82.856, card: true, draw: u => hit(C_BEND, u, { inv: true }) },
  // cards the staff card, two beats
  { t: 83.324, card: true, draw: u => {
    fill(BLACK);
    const a = 1 - prog(u, 0.7, 0.93);
    blit(C_STAFF[0], 144, 108, { c: WHITE, a });
    blit(C_STAFF[1], 1296, 540, { c: WHITE, ax: 1, ay: 0.5, a });
  } },
  // ==== logo 84.26-87.32: the mark returns, completes, and loops once ====
  // The outro: the mark returns in dots, the pen joins them, and it plays
  // its loop once, to rest before the ∀ card.
  { t: 84.26, draw: u => {
    const pen = at(u, 0.4, 0.95);
    logo({ z: 760, c: YEL, hot: RED, w: 2.6, glow: 8, dots: prog(u, 0, 0.6), pen, nodes: pen, fill: 0.3 * pen, loop: Math.max(0, u - 0.95) / 2.1 });
  } },
  // ==== cards the end: the ∀ card and the plate (kept) ====
  { t: 87.32, card: true, draw: () => { fill(WHITE); blit(C_FORALL, 720, 540, { c: BLACK, ax: 0.5, ay: 0.5 }); } },
  { t: 87.59, draw: u => {
    plate(u);
    for (const [i, y] of [[0, 300], [1, 420], [2, 660], [3, 780]]) {
      blit(T_END[i], 146, y + 3, { c: BLACK, a: 0.6 });
      blit(T_END[i], 144, y, { c: hex(0xd8dcb0) });
    }
    fill(BLACK, prog(u, 1.75, 2.01));
  } },
];


let CLIP0 = 0, CLIP1 = 1e9;

function clip_x(x0: number, x1: number, f: () => void): void {
  CLIP0 = Math.max(0, Math.round(x0 * S));
  CLIP1 = Math.round(x1 * S);
  f();
  CLIP0 = 0;
  CLIP1 = 1e9;
}
// Draws frame f into buf; answers whether it is a clean card (no tube).
function frame(f: number): boolean {
  const t = f / FPS;
  buf.set(base);
  let i = 0;
  while (i + 1 < CUTS.length && CUTS[i + 1].t <= t) ++i;
  CUTS[i].draw(t - CUTS[i].t, t);
  if (CUTS[i].card) return true;
  // Dust: a few specks and, now and then, a hair.
  for (let k = 0; k < 3; ++k) {
    if (hash(f * 3 + k, 40) < 0.3) disc(hash(f * 3 + k, 41) * 1440, hash(f * 3 + k, 42) * 1080, 1 + hash(f * 3 + k, 43) * 2.5, TXT, 0.45);
  }
  if (hash(f, 44) < 0.03) {
    const x = hash(f, 45) * 1440, y = hash(f, 46) * 1080;
    line(x, y, x + 30 * (hash(f, 47) - 0.5), y + 50, 1.2, TXT, 0.35);
  }
  return false;
}

// Output
// ======

function run(cmd: string[]): Buffer {
  const got = Bun.spawnSync(cmd, { stdout: "pipe", stderr: "inherit" });
  if (got.exitCode !== 0) throw new Error("failed: " + cmd.join(" "));
  return got.stdout as Buffer;
}

// The tube: a soft vignette on the picture (a quarter darker at the rim).
const VIG = new Float32Array(W * H);
for (let y = 0; y < H; ++y) {
  for (let x = 0; x < W; ++x) {
    const d = Math.hypot((x / W - 0.5) * 2, (y / H - 0.5) * 2) / Math.SQRT2;
    VIG[y * W + x] = 1 - 0.3 * Math.pow(clamp((d - 0.35) / 0.65), 1.4);
  }
}

// A normalized Gaussian kernel of deviation sd.
function gauss(sd: number): Float32Array {
  const r = Math.max(1, Math.ceil(2.5 * sd)), k = new Float32Array(2 * r + 1);
  for (let i = -r; i <= r; ++i) k[i + r] = Math.exp(-i * i / (2 * sd * sd));
  const sum = k.reduce((a, b) => a + b);
  return k.map(v => v / sum);
}

// Convolves an RGB image of w by h with kernel k along (dx, dy).
function conv(src: Float32Array, dst: Float32Array, w: number, h: number, k: Float32Array, dx: number, dy: number): void {
  const r = k.length >> 1;
  for (let y = 0; y < h; ++y) for (let x = 0; x < w; ++x) {
    let a = 0, b = 0, c = 0;
    for (let i = -r; i <= r; ++i) {
      const xx = Math.min(w - 1, Math.max(0, x + i * dx)), yy = Math.min(h - 1, Math.max(0, y + i * dy));
      const j = (yy * w + xx) * 3, q = k[i + r];
      a += src[j] * q; b += src[j + 1] * q; c += src[j + 2] * q;
    }
    const o = (y * w + x) * 3;
    dst[o] = a; dst[o + 1] = b; dst[o + 2] = c;
  }
}

// The phosphor, in three layers:
// - the beam: the frame, softened a pixel, more along the scan (x);
// - the bleed (half size, 4 px blur): the color of the picture, its red and
//   blue a little misconverged; the frame keeps the beam's luma but takes
//   the bleed's chroma, so thin lines burn pale at the core, tinted around;
// - the halo: the bleed plus the glow (quarter size, blurred twice by
//   boxes, 11 px), screened over the frame.
const HW = Math.ceil(W / 2), HH = Math.ceil(H / 2), GW = Math.ceil(W / 4), GH = Math.ceil(H / 4);
const BEAM_X = gauss(1.2 * S), BEAM_Y = gauss(0.9 * S), K_BLEED = gauss(2.2 * S);
const P_SOFT  = new Float32Array(W * H * 3);
const P_BLEED = new Float32Array(HW * HH * 3), P_HALO = new Float32Array(HW * HH * 3), P_HTMP = new Float32Array(HW * HH * 3);
const glow = new Float32Array(GW * GH * 3), tmp = new Float32Array(GW * GH * 3);
const luma = (r: number, g: number, b: number) => 0.299 * r + 0.587 * g + 0.114 * b;

// A box blur of radius r quarter pixels along (dx, dy), on quarter-size images.
function blur(src: Float32Array, dst: Float32Array, dx: number, dy: number, r: number): void {
  for (let y = 0; y < GH; ++y) for (let x = 0; x < GW; ++x) {
    let a = 0, b = 0, c = 0, n = 0;
    for (let k = -r; k <= r; ++k) {
      const xx = x + k * dx, yy = y + k * dy;
      if (xx < 0 || yy < 0 || xx >= GW || yy >= GH) continue;
      const j = (yy * GW + xx) * 3; a += src[j]; b += src[j + 1]; c += src[j + 2]; ++n;
    }
    const i = (y * GW + x) * 3; dst[i] = a / n; dst[i + 1] = b / n; dst[i + 2] = c / n;
  }
}

function phosphor(): void {
  conv(buf, P_SOFT, W, H, BEAM_X, 1, 0); conv(P_SOFT, buf, W, H, BEAM_Y, 0, 1);
  const mis = Math.max(1, Math.round(S));
  for (let y = 0; y < HH; ++y) for (let x = 0; x < HW; ++x) {
    const o = (y * HW + x) * 3;
    for (let c = 0; c < 3; ++c) {
      const sx = 2 * x + (c === 0 ? mis : c === 2 ? -mis : 0);
      let v = 0;
      for (let k = 0; k < 4; ++k) {
        const px = Math.min(W - 1, Math.max(0, sx + (k & 1))), py = Math.min(H - 1, 2 * y + (k >> 1));
        v += buf[(py * W + px) * 3 + c];
      }
      P_HTMP[o + c] = v / 4;
    }
  }
  conv(P_HTMP, P_HALO, HW, HH, K_BLEED, 1, 0); conv(P_HALO, P_BLEED, HW, HH, K_BLEED, 0, 1);
  for (let y = 0; y < GH; ++y) for (let x = 0; x < GW; ++x) {
    const o = (y * GW + x) * 3, i = (Math.min(HH - 1, 2 * y) * HW + Math.min(HW - 1, 2 * x)) * 3;
    glow[o] = P_BLEED[i]; glow[o + 1] = P_BLEED[i + 1]; glow[o + 2] = P_BLEED[i + 2];
  }
  const r = Math.max(1, Math.round(3 * S));
  blur(glow, tmp, 1, 0, r); blur(tmp, glow, 0, 1, r);
  blur(glow, tmp, 1, 0, r); blur(tmp, glow, 0, 1, r);
  for (let y = 0; y < HH; ++y) {
    const gy = Math.min(GH - 1.001, y / 2), y0 = Math.floor(gy), fy = gy - y0;
    for (let x = 0; x < HW; ++x) {
      const gx = Math.min(GW - 1.001, x / 2), x0 = Math.floor(gx), fx = gx - x0, k = (y0 * GW + x0) * 3, o = (y * HW + x) * 3;
      for (let c = 0; c < 3; ++c) {
        const g = lerp(lerp(glow[k + c], glow[k + 3 + c], fx), lerp(glow[k + GW * 3 + c], glow[k + GW * 3 + 3 + c], fx), fy);
        P_HALO[o + c] = 0.25 * P_BLEED[o + c] + 0.35 * g;
      }
    }
  }
}

// The grain: a tile of fine luma noise, shifted every frame.
const GRAIN = Float32Array.from({ length: 1 << 16 }, (_, k) => (hash(k, 60) + hash(k, 61) - 1) * 0.035);

async function frames_pipe(fs_: number[], args: string[]): Promise<void> {
  const ff = Bun.spawn(["ffmpeg", "-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgb24",
    "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", ...args], { stdin: "pipe", stderr: "inherit" });
  const out = new Uint8Array(W * H * 3);
  for (const f of fs_) {
    const clean = frame(f);
    if (clean) {
      for (let i = 0; i < W * H; ++i) {
        const k = i * 3;
        out[k] = clamp(buf[k]) * 255; out[k + 1] = clamp(buf[k + 1]) * 255; out[k + 2] = clamp(buf[k + 2]) * 255;
      }
    } else {
      phosphor();
      // Gate weave: the picture drifts a pixel or two, frame to frame.
      const wx = Math.round((hash(f, 51) - 0.5) * 4 * S), wy = Math.round((hash(f, 52) - 0.5) * 2 * S);
      const flick = 1 + 0.02 * (hash(f, 50) - 0.5);
      const gx = Math.floor(hash(f, 53) * 256), gy = Math.floor(hash(f, 54) * 256);
      for (let y = 0; y < H; ++y) {
        const sy = Math.min(H - 1, Math.max(0, y + wy));
        const hy = Math.min(HH - 1.001, sy / 2), y0 = Math.floor(hy), fy = hy - y0;
        for (let x = 0; x < W; ++x) {
          const i = y * W + x, v = flick * VIG[i], k = i * 3;
          if (v === 0) { out[k] = out[k + 1] = out[k + 2] = 0; continue; }
          const sx = Math.min(W - 1, Math.max(0, x + wx)), j = (sy * W + sx) * 3;
          const hx = Math.min(HW - 1.001, sx / 2), x0 = Math.floor(hx), fx = hx - x0, h = (y0 * HW + x0) * 3;
          const w00 = (1 - fx) * (1 - fy), w01 = fx * (1 - fy), w10 = (1 - fx) * fy, w11 = fx * fy;
          const n = [0, 0, 0], l = [0, 0, 0];
          for (let c = 0; c < 3; ++c) {
            const a = h + c, b = a + HW * 3;
            n[c] = P_BLEED[a] * w00 + P_BLEED[a + 3] * w01 + P_BLEED[b] * w10 + P_BLEED[b + 3] * w11;
            l[c] = P_HALO[a] * w00 + P_HALO[a + 3] * w01 + P_HALO[b] * w10 + P_HALO[b + 3] * w11;
          }
          // The beam's luma, the bleed's chroma, the halo screened, the vignette, the grain.
          const ys = luma(buf[j], buf[j + 1], buf[j + 2]), yn = luma(n[0], n[1], n[2]);
          const g = GRAIN[((y + gy) & 255) * 256 + ((x + gx) & 255)];
          for (let c = 0; c < 3; ++c) {
            const p = ys + n[c] - yn;
            out[k + c] = clamp((1 - (1 - p) * (1 - l[c])) * v + g) * 255;
          }
        }
      }
    }
    ff.stdin.write(out);
    await ff.stdin.flush();
  }
  ff.stdin.end();
  if (await ff.exited !== 0) throw new Error("ffmpeg failed");
}

async function main(): Promise<void> {
  const [cmd, ...args] = process.argv.slice(2);
  sprite_load();
  const total = Math.round(END * FPS);
  if (cmd === "sheet") {
    // A contact sheet: frames from t0 to t1 every step seconds, 6 per row.
    const [t0, t1, step] = args.map(Number);
    const fs_: number[] = [];
    for (let t = t0; t < t1; t += step) fs_.push(Math.round(t * FPS));
    const rows = Math.ceil(fs_.length / 6);
    await frames_pipe(fs_, ["-vf", `scale=480:-1,tile=6x${rows}`,
      "-frames:v", "1", path.join(TMP, `sheet-${t0}-${t1}.png`)]);
  } else if (cmd === "part") {
    const [i, n] = args.map(Number);
    const a = Math.floor(total * i / n), b = Math.floor(total * (i + 1) / n);
    await frames_pipe(Array.from({ length: b - a }, (_, k) => a + k),
      ["-vf", "format=yuv420p", "-c:v", "libx264", "-preset", "medium", "-crf", "23", path.join(TMP, `part${i}.mp4`)]);
  } else if (cmd === "join") {
    const list = path.join(TMP, "parts.txt");
    fs.writeFileSync(list, Array.from({ length: PARTS }, (_, i) => `file 'parts/part${i}.mp4'`).join("\n") + "\n");
    run(["ffmpeg", "-v", "error", "-y", "-f", "concat", "-safe", "0", "-i", list, "-i", SONG,
      "-map", "0:v", "-map", "1:a", "-c:v", "copy", "-c:a", "aac", "-b:a", "192k",
      "-af", `afade=t=out:st=${END - 0.8}:d=0.8`, "-t", String(END), "-movflags", "+faststart", OUT]);
  }
}

await main();
