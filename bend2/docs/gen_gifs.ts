#!/usr/bin/env node
// The README's two charts, media/runtime.gif and media/checker.gif: the landing
// page's bar chart, one bench per page, paging forever. Same look and rhythm as
// the site's front/site/index.html chart(): the title above, the seconds (and
// the speedup over one core) above each bar, the names below, Bend in purple,
// the others in gray, a checker timeout hatched at full height with ">5 min"
// above, a row of dots for the position. A page turn glides every bar to its
// new height (0.6 s, ease-out) and crossfades its number; then the page holds
// 2.9 s as ONE gif frame, so each bench is 3.5 s and the files stay small.
// Drawn at 2x (1280 px wide) on a TRANSPARENT canvas, in colors that read on
// the light and the dark GitHub, which embeds them at 640. This script measures
// NOTHING: every bar is a pin read by gen_charts.ts, the titles are the site's.
//
// It needs node >= 22.18, the canvas package, ffmpeg and Menlo, which
// this Mac lacks: render on cluster-9d, where ~/film has all four.
//
//   bun bend2/docs/gen_pins.ts && bun bend2/docs/gen_charts.ts
//   tar cf - bend2/docs/gen_*.ts bench/*/_pin_/apple_m4_max.txt \
//     | ssh -J cluster cluster-9d 'cd film && tar xf -'
//   tar cf - -C .. bend-lang.com/front/site/index.html \
//     | ssh -J cluster cluster-9d 'tar xf -'   # beside film: gen_charts.ts looks there
//   ssh -J cluster cluster-9d 'cd film && \
//     PATH=/usr/local/node/bin:$PATH:$HOME/film node bend2/docs/gen_gifs.ts'
//   scp -o ProxyJump=cluster 'cluster-9d:film/media/*.gif' media/

import { createCanvas } from "canvas";

import * as charts from "./gen_charts.ts";

// Types
// =====

type Bar = { name: string; secs: number; bend?: boolean; over?: boolean;
  mul?: string };
type Page = { title: string; bars: Bar[] };

// Constants
// =========

// the site's chart at 16 px per em: its bars are .8 em, 10 em tall at
// most, in a 35 em box, 13.5 em high here; the canvas is that, at 2x
const EM = 16;
const BAR_EM = 0.8 * EM;
const W = 40 * EM;
const H = 21 * EM;
const K = 2;
const BOX_BOT = 16.1 * EM;
const BAR_MAX = 10 * BAR_EM;
const GAP = 0.7 * EM;
const NAME_H = 2.6 * BAR_EM;
const FPS = 25;
const GLIDE = 0.6;
const HOLD = 2.9;
const INK = "#87847d";
const GRAY = "#a5a29a";
const PURPLE = "#8b83b5";
const GREEN = "#7e9a5e";
const MONO = "Menlo, monospace";
const MACHINE = "Apple M4 Max · lower is better";

const cv = createCanvas(W * K, H * K);
const cx = cv.getContext("2d");
cx.scale(K, K);

function secs(s: number): string {
  return (s >= 10 ? s.toFixed(1) : s.toFixed(2)) + "s";
}

function times(x: number): string {
  return (x >= 10 ? String(Math.round(x)) : x.toFixed(1)) + "x";
}

function clamp(x: number): number {
  return x < 0 ? 0 : x > 1 ? 1 : x;
}

function ease_out(x: number): number {
  return 1 - Math.pow(1 - clamp(x), 3);
}

// Pages
// =====

// every bench, best to worst by Bend's combined advantage: (one core
// over the best twin) x (GPU over all cores)
function pages_runtime(): Page[] {
  const titles = charts.front_titles();
  const score = (r: charts.RunRow): number =>
    (r.seq / Math.min(r.c, r.ts, r.lean)) * (r.gpu / r.par);
  return charts.pin_runtime().sort((a, b) => score(a) - score(b))
    .map((r): Page => ({ title: titles[r.bench], bars: [
      { name: "TypeScript", secs: r.ts },
      { name: "Lean", secs: r.lean },
      { name: "C", secs: r.c },
      { name: "Bend\n1 core", secs: r.seq, bend: true, mul: "1x" },
      { name: "Bend\n16 cores", secs: r.par, bend: true,
        mul: times(r.seq / r.par) },
      { name: "Bend\nGPU", secs: r.gpu, bend: true,
        mul: times(r.seq / r.gpu) },
    ] }));
}

// every checker family, ordered by Bend's lead over the best rival
function pages_checker(): Page[] {
  const titles = charts.front_titles();
  const names = ["Isabelle", "Agda", "Lean", "Rocq", "Bend"];
  const rows = charts.pin_checker();
  const pages = [...new Set(rows.map((r) => r.family))].map((family) => {
    const mine = rows.filter((r) => r.family === family);
    return { title: titles[family + "_" + String(mine[0].n)],
      bars: mine.map((r, i): Bar => ({ name: names[i], secs: r.secs,
        over: r.over, bend: r.lang === "bend" })) };
  });
  const lead = (p: Page): number =>
    Math.min(...p.bars.slice(0, 4).map((b) => b.secs)) / p.bars[4].secs;
  return pages.sort((a, b) => lead(b) - lead(a));
}

// a bar's height, as a share of the tallest: a timeout fills the chart
// and halves the rest
function page_heights(p: Page): number[] {
  const live = Math.max(...p.bars.map((b) => b.over === true ? 0 : b.secs));
  const vmax = live * (p.bars.some((b) => b.over === true) ? 2 : 1);
  return p.bars.map((b) => b.over === true ? 1 : b.secs / vmax);
}

function bar_label(b: Bar): string[] {
  const value = b.over === true ? ">5 min" : secs(b.secs);
  return b.mul === undefined ? [value] : [b.mul, value];
}

// Draw
// ====

function text(s: string, x: number, y: number, size: number, color: string,
  bold = false): void {
  cx.font = (bold ? "bold " : "") + String(size) + "px " + MONO;
  cx.fillStyle = color;
  cx.textAlign = "center";
  cx.textBaseline = "middle";
  cx.fillText(s, x, y);
}

function hatch(x: number, y: number, w: number, h: number): void {
  cx.save();
  cx.beginPath();
  cx.rect(x, y, w, h);
  cx.clip();
  cx.strokeStyle = GRAY;
  cx.lineWidth = 5.5;
  cx.beginPath();
  for (let d = -h; d < w; d += 12.7) {
    cx.moveTo(x + d, y + h);
    cx.lineTo(x + d + h, y);
  }
  cx.stroke();
  cx.restore();
}

// a bar's number, and its speedup in green above it, ending 5 px over
// the bar's top
function label(lines: string[], alpha: number, x: number, top: number): void {
  cx.globalAlpha = alpha;
  lines.forEach((line, ln): void => {
    const y = top - 5 - (lines.length - ln - 0.5) * 1.25 * BAR_EM;
    text(line, x, y, BAR_EM, lines.length === 2 && ln === 0 ? GREEN : INK);
  });
  cx.globalAlpha = 1;
}

// one frame of the turn from page a to page b, t seconds in: the bars
// glide, a's numbers fade out as b's fade in, the rest is b's
function frame(a: Page, b: Page, t: number, at: number, n: number): void {
  const ha = page_heights(a);
  const hb = page_heights(b);
  const bw = (35 * EM - (b.bars.length - 1) * GAP) / b.bars.length;
  const x0 = (W - 35 * EM) / 2;
  cx.clearRect(0, 0, W, H);
  text(b.title, W / 2, 1.8 * EM, EM, INK, true);
  b.bars.forEach((bar, i): void => {
    const x = x0 + i * (bw + GAP);
    const base = BOX_BOT - NAME_H - 6;
    const h = Math.max(2, BAR_MAX * (ha[i] + (hb[i] - ha[i]) * ease_out(t / GLIDE)));
    if (bar.over === true) {
      hatch(x, base - h, bw, h);
    } else {
      cx.fillStyle = bar.bend === true ? PURPLE : GRAY;
      cx.fillRect(x, base - h, bw, h);
    }
    const la = bar_label(a.bars[i]);
    const lb = bar_label(bar);
    if (la.join() === lb.join()) {
      label(lb, 1, x + bw / 2, base - h);
    } else {
      label(la, 1 - clamp(t / 0.2), x + bw / 2, base - h);
      label(lb, clamp((t - 0.22) / 0.4), x + bw / 2, base - h);
    }
    bar.name.split("\n").forEach((line, ln): void => {
      const y = BOX_BOT - NAME_H + (ln + 0.5) * 1.3 * BAR_EM;
      text(line, x + bw / 2, y, BAR_EM, bar.bend === true ? PURPLE : INK, bar.bend === true);
    });
  });
  for (let i = 0; i < n; i++) {
    const x = W / 2 + (i - (n - 1) / 2) * 1.6 * 0.87 * EM;
    text(i === at ? "●" : "○", x, 17.4 * EM, 0.87 * EM, i === at ? PURPLE : INK);
  }
  text(MACHINE, W / 2, 19.5 * EM, 0.87 * EM, INK);
}

// Gif
// ===

// every turn is GLIDE seconds of frames at FPS; the last frame of each
// turn is the page itself, and lasts HOLD seconds more
function gif(name: string, pages: Page[]): void {
  const steps = Math.round(GLIDE * FPS);
  const shots: [[Page, Page, number, number], number][] = [];
  pages.forEach((page, at): void => {
    const prev = pages[(at + pages.length - 1) % pages.length];
    for (let k = 1; k <= steps; k++) {
      shots.push([[prev, page, k / FPS, at], 1 / FPS + (k === steps ? HOLD : 0)]);
    }
  });
  const got = charts.gif_write(name, cv,
    ([a, b, t, at]) => frame(a, b, t, at, pages.length), shots);
  process.stdout.write("wrote " + got.out + ": " + String(pages.length)
    + " pages, " + String(got.frames) + " frames, "
    + String(got.bytes) + " bytes\n");
}

// Main
// ====

gif("runtime", pages_runtime());
gif("checker", pages_checker());
