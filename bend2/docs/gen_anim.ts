#!/usr/bin/env node
// The README's two page animations, media/hero.gif and media/parallel.gif.
//
// hero.gif is the landing page's hero: the word Bend, the purple block
// blinking after it, and the one line under it. parallel.gif is the
// landing page's pow2 canvas: the call splits in two, in four, ... until
// one task sits on each of 64 x 64 cores; every core works its task; the
// results fold back pairwise; the view zooms on the one cell that holds
// the answer.
//
// Everything is drawn on a TRANSPARENT canvas, in the palette of the
// landing page, so the tiles float on the reader's own page, light or
// dark, and a reader who sees both the page and the README sees one
// design. A gif has 1-bit alpha, so nothing here fades: draw or do not
// draw. The hero needs an ink per theme, so it has one file per theme.
//
// It needs node >= 22.18, the canvas package, ffmpeg and Menlo, which
// this Mac lacks: render on cluster-9d, where ~/film has all four.
//
//   tar cf - bend2/docs/gen_anim.ts bend2/docs/gen_charts.ts \
//     | ssh -J cluster cluster-9d 'cd film && tar xf -'
//   ssh -J cluster cluster-9d 'cd film && \
//     PATH=/usr/local/node/bin:$PATH:$HOME/film node bend2/docs/gen_anim.ts'
//   scp -o ProxyJump=cluster 'cluster-9d:film/media/{hero,parallel}.gif' media/

import { createCanvas } from "canvas";

import { gif_write } from "./gen_charts.ts";

// Constants
// =========

const MONO = "Menlo, monospace";
const PURPLE = "#8b83b5";
const BG = "#f2eee7";
const INK = "#5e5787";
const FPS = 25;

// Lib
// ===

function clamp(x: number): number {
  return x < 0 ? 0 : x > 1 ? 1 : x;
}

function ease(x: number): number {
  const u = clamp(x);
  return u * u * (3 - 2 * u);
}

function lerp(a: number, b: number, x: number): number {
  return a + (b - a) * x;
}

function mix(a: string, b: string, f: number): string {
  const pick = (h: string, i: number): number =>
    parseInt(h.slice(1 + 2 * i, 3 + 2 * i), 16);
  const one = (i: number): string =>
    String(Math.round(lerp(pick(a, i), pick(b, i), clamp(f))));
  return "rgb(" + one(0) + "," + one(1) + "," + one(2) + ")";
}

function gif_report(got: { out: string; frames: number; bytes: number }): void {
  process.stdout.write("wrote " + got.out + ": " + String(got.frames)
    + " frames, " + String(got.bytes) + " bytes\n");
}

// Hero
// ====

// The landing page's hero, as the film's title card draws it: Bend, the
// purple block blinking after it, the pitch with its bold words in ink
// and the rest dim, and the three claims under it. Two frames, the block
// on and off, as the page blinks it every 1.1 s. One file per GitHub
// theme, since a gif cannot follow the reader's: the page picks with
// <picture> and prefers-color-scheme
function hero(name: string, ink: string, dim: string): void {
  const W = 1120;
  const H = 250;
  const TITLE = 88;
  const PITCH: [string, boolean][] = [["a ", false], ["fast", true],
    [" language that ", false], ["blocks AI mistakes", true],
    [" via ", false], ["proof", true]];
  const DOT = " \u00b7 ";
  const CLAIMS: [string, boolean][] = [["C", true], [" speed", false],
    [DOT, false], ["CUDA", true], [" parallelism", false], [DOT, false],
    ["Lean", true], [" proofs", false], [DOT, false], ["Python", true],
    [" syntax", false]];
  const cv = createCanvas(W, H);
  const cx = cv.getContext("2d");
  const font = (size: number, bold: boolean): string =>
    (bold ? "bold " : "") + String(size) + "px " + MONO;
  // the run's width, then its parts, left to right from the centre
  function run(parts: [string, boolean][], size: number, y: number): void {
    let w = 0;
    for (const [s, bold] of parts) {
      cx.font = font(size, bold);
      w += cx.measureText(s).width;
    }
    let x = (W - w) / 2;
    for (const [s, bold] of parts) {
      cx.font = font(size, bold);
      cx.fillStyle = bold ? ink : dim;
      cx.fillText(s, x, y);
      x += cx.measureText(s).width;
    }
  }
  function draw(t: number): void {
    cx.clearRect(0, 0, W, H);
    cx.textBaseline = "alphabetic";
    cx.textAlign = "left";
    cx.font = font(TITLE, true);
    const bw = TITLE * 0.5;
    const tw = cx.measureText("Bend").width + TITLE * 0.11 + bw;
    const tx = (W - tw) / 2;
    const ty = 100;
    cx.fillStyle = ink;
    cx.fillText("Bend", tx, ty);
    if (t < 0.55) {
      cx.fillStyle = PURPLE;
      cx.fillRect(tx + tw - bw, ty - TITLE * 0.86, bw, TITLE * 0.92);
    }
    run(PITCH, TITLE * 0.36, ty + TITLE * 0.89);
    run(CLAIMS, TITLE * 0.264, ty + TITLE * 1.39);
  }
  gif_report(gif_write(name, cv, draw, [[0, 0.55], [0.6, 0.55]]));
}

// Parallel
// ========

// pow2(20) splits in two, in four, ..., one task per core; each core
// works its own call; the results fold back pairwise into the top-left
// cell, which the view then zooms on. It is the landing page's canvas,
// beat for beat, on the page's own paper and palette, so the page and
// the README read as one animation.
function parallel(): void {
  const W = 620;
  const N = 64;
  const MID = 32;
  const LEVELS = 12;
  const D0 = 0.5;
  const HOLD1 = 0.5;
  const EPRE = 0.4;
  const DIVE = 1.8;
  const EHOLD = 2.5;
  const BACK = 0.8;
  const RHOLD = 0.7;
  const ESTEP = 0.24;
  const ZMAX = 64;
  const ZLEN = 1.4;
  // what one core runs: pow2(8), call by call, the way a core runs it
  const STEPS = 10;
  const TRACE = ["pow2(8)"];
  for (let d = 7; d >= 0; d--) {
    TRACE.push(String(256 - (1 << (d + 1)) + (1 << d)) + "+pow2("
      + String(d) + ")");
  }
  TRACE.push("256");
  const TILES = ["#e4e1ec", "#d2cee1", "#bfb9d5", "#aca5c9", "#9a92be",
    "#8b83b5", "#6e6694"];
  const SKY = TILES[0];
  const split_dur = (k: number): number => 0.8 * Math.pow(0.8, k);
  const fold_dur = (j: number): number => 0.4 * Math.pow(0.85, j);
  const rung = (t: number): string => {
    const i = Math.min(Math.floor(t), TILES.length - 2);
    return mix(TILES[i], TILES[i + 1], t - i);
  };
  const HEAT: string[] = [];
  for (let i = 0; i <= 16; i++) {
    HEAT.push(rung(2 * i / 16));
  }
  const dims = (k: number): [number, number] =>
    [1 << Math.ceil(k / 2), 1 << Math.floor(k / 2)];
  const rnd = (i: number): number => {
    const s = Math.sin(i * 12.9898) * 43758.5453;
    return s - Math.floor(s);
  };
  // no core starts before the camera has landed, so nothing changes
  // under the reader mid-dive; then they scatter by a beat
  const phase = (c: number, r: number): number => EPRE + DIVE
    + (c === MID && r === MID ? 0 : rnd(c * 7919 + r * 104729) * 0.5);
  let split_end = D0;
  for (let k = 0; k < LEVELS; k++) {
    split_end += split_dur(k);
  }
  const E0 = split_end + HOLD1;
  const R0 = E0 + EPRE + DIVE + EHOLD + BACK + RHOLD;
  let fold_end = R0;
  for (let j = 0; j < LEVELS; j++) {
    fold_end += fold_dur(j);
  }
  const Z0 = fold_end + 0.4;
  const Z1 = Z0 + ZLEN;
  const fold_at = (u: number): number => {
    let j = 0;
    let t = R0;
    while (j < LEVELS && u >= t + fold_dur(j)) {
      t += fold_dur(j);
      j++;
    }
    return j < LEVELS ? j + clamp((u - t) / fold_dur(j)) : LEVELS;
  };
  const cv = createCanvas(W, W);
  const cx = cv.getContext("2d");
  const gap = (s: number): number => Math.min(Math.max(s * 0.1, 1), 6);
  const fsz = (w: number, h: number): number =>
    Math.min(Math.min(w, h) * 0.17, 50);
  function cell(x: number, y: number, w: number, h: number,
    fill: string): void {
    const g = gap(Math.min(w, h));
    cx.fillStyle = fill;
    cx.fillRect(x + g / 2, y + g / 2, w - g, h - g);
  }
  function label(s: string, x: number, y: number, w: number, h: number,
    fs: number, ink: string, a: number): void {
    if (fs < 7 || a <= 0) {
      return;
    }
    cx.font = "bold " + String(fs) + "px " + MONO;
    if (cx.measureText(s).width > w * 0.9) {
      return;
    }
    cx.globalAlpha = a;
    cx.fillStyle = ink;
    cx.textAlign = "center";
    cx.fillText(s, x + w / 2, y + h / 2 + fs * 0.36);
    cx.globalAlpha = 1;
  }
  // the k-th generation of tasks, with the seam of the cut that made it
  // m of the way open. No cell moves: a cut is a seam that opens down
  // the middle of its parent, so the picture is whole at every frame.
  // The label divides with the cell: at the cut, two copies of the new
  // call sit on the old one, and they ride apart into their own halves
  function slots(k: number, m: number): void {
    const [cols, rows] = dims(k);
    const w = W / cols;
    const h = W / rows;
    const [pc, pr] = k > 0 ? dims(k - 1) : [1, 1];
    const pw = W / pc;
    const ph = W / pr;
    const vert = cols > pc;
    const g = lerp(gap(Math.min(pw, ph)), gap(Math.min(w, h)), m) / 2;
    cx.fillStyle = SKY;
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        const l = vert && c % 2 === 1 ? g * m : g;
        const q = vert && c % 2 === 0 ? g * m : g;
        const t = !vert && r % 2 === 1 ? g * m : g;
        const b = !vert && r % 2 === 0 ? g * m : g;
        cx.fillRect(c * w + l, r * h + t, w - l - q, h - t - b);
      }
    }
    // the label cannot dissolve on a gif, so the new call takes the cell
    // when the seam is half open, and the old one holds until then
    const grid = (cs: number, rs: number, name: string, a: number): void => {
      const gw = W / cs;
      const gh = W / rs;
      for (let r = 0; r < rs; r++) {
        for (let c = 0; c < cs; c++) {
          label(name, c * gw, r * gh, gw, gh, fsz(gw, gh), INK, a);
        }
      }
    };
    if (k > 0 && m < 0.5) {
      grid(pc, pr, "pow2(" + String(21 - k) + ")", 1);
    } else {
      grid(cols, rows, "pow2(" + String(20 - k) + ")", 1);
    }
  }

  function draw(u: number): void {
    const cs = W / N;
    cx.clearRect(0, 0, W, W);
    if (u < E0) {                                                  // split
      let k = 0;
      let s = D0;
      while (k < LEVELS && u >= s + split_dur(k)) {
        s += split_dur(k);
        k++;
      }
      const d = split_dur(k);
      const cut = Math.min(0.55, d * 0.75);
      const p = k < LEVELS ? ease((u - (s + d - cut)) / cut) : 0;
      if (p > 0) {
        slots(k + 1, p);
      } else {
        slots(k, 1);
      }
    } else if (u < R0) {                                           // eval
      const v = u - E0;
      const p = v < EPRE ? 0 : v < EPRE + DIVE ? ease((v - EPRE) / DIVE)
        : v < EPRE + DIVE + EHOLD ? 1
        : 1 - ease((v - EPRE - DIVE - EHOLD) / BACK);
      const zs = cs * Math.exp(p * Math.log(ZMAX));
      const o = W / 2 - (MID + 0.5) * zs;
      for (let r = 0; r < N; r++) {
        for (let c = 0; c < N; c++) {
          const x = o + c * zs;
          const y = o + r * zs;
          if (x > W || y > W || x + zs < 0 || y + zs < 0) {
            continue;
          }
          const j = Math.min(STEPS - 1,
            Math.max(0, Math.floor((v - phase(c, r)) / ESTEP)));
          cell(x, y, zs, zs, HEAT[Math.round(j * 16 / (STEPS - 1))]);
          label(TRACE[j], x, y, zs, zs, fsz(zs, zs), INK, 1);
        }
      }
    } else if (u < Z0) {                                           // fold
      const f = fold_at(u);
      const j = Math.floor(f);
      const m = ease(f - j);
      const k = LEVELS - j;
      const [w, h] = dims(k);
      const vert = k % 2 === 1;
      const fill = rung(2 + 4 * f / LEVELS);
      for (let r = 0; r < h; r++) {
        for (let c = 0; c < w; c++) {
          cell((vert ? lerp(c, c / 2, m) : c) * cs,
            (vert ? r : lerp(r, r / 2, m)) * cs, cs, cs, fill);
        }
      }
    } else {                                                       // zoom
      const zs = cs * Math.pow(N, ease((u - Z0) / ZLEN));
      cell(0, 0, zs, zs, TILES[6]);
      label("1048576", 0, 0, zs, zs, fsz(zs, zs), BG, u > Z1 - 0.1 ? 1 : 0);
    }
  }
  const shots: [number, number][] = [];
  const end = Z1 + 0.9;
  for (let i = 0; i * (1 / FPS) < end; i++) {
    shots.push([i / FPS, 1 / FPS]);
  }
  shots[shots.length - 1][1] = 1.8;
  gif_report(gif_write("parallel", cv, draw, shots));
}

// Game
// ====

// The landing page's three acts, one gif each. The level is a 12 x 8 map:
// the flag sits in a room sealed by two walls, the player starts at
// (8,5). Act one: the player walks up and bumps the room's wall, so the
// law holds. Act two: the AI added the wrap feature and no law stopped
// it, so the player walks off the right edge, in at the left, and takes
// the flag. Act three: the law forced the AI to seal the far edge, so
// the same walk stops there.
const GW = 12;
const GH = 8;
const TILE = 64;
const STEP = 0.2;
const BUMP = 0.4;
const BUMPS = 3;
const LEAD = 0.6;
const HOLD = 1.8;
const PAL = { floor: ["#ebe8e2", "#e1ded7"], wall: "#8e8b87", cap: "#a8a5a1",
  hit: "#d9a39c", hitcap: "#e8c4bf", pole: "#87847d", cloth: "#7e9a5e",
  skin: "#78c0e3", eye: "#2f3b4c", win: "#f6e4e1", rim: "#dfa9a2",
  winink: "#c46a60" };

type Act = { name: string; far: boolean; route: [number, number][];
  bump?: [number, string]; win?: boolean; pop?: string };

// the walk up the column, then along the row
function up(x: number, y0: number, y1: number): [number, number][] {
  const p: [number, number][] = [];
  for (let y = y0; y >= y1; y--) {
    p.push([x, y]);
  }
  return p;
}

const ACTS: Act[] = [
  { name: "game_law", far: false,
    route: up(8, 5, 1).concat([[7, 1], [6, 1], [5, 1], [4, 1]]),
    bump: [-1, "3,1"] },
  { name: "game_bug", far: false,
    route: up(8, 5, 1).concat([[9, 1], [10, 1], [11, 1], [12, 1], [-1, 1],
      [0, 1], [1, 1]]), win: true, pop: "YOU WON !?" },
  { name: "game_law_kept", far: true,
    route: up(8, 5, 1).concat([[9, 1], [10, 1]]), bump: [1, "11,1"] },
];

function game(act: Act): void {
  const W = GW * TILE;
  const H = GH * TILE;
  const cv = createCanvas(W, H);
  const cx = cv.getContext("2d");
  const near = (a: [number, number], b: [number, number]): boolean =>
    Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) <= 1;
  const walk = act.route.reduce((t, c, i) =>
    i > 0 && near(c, act.route[i - 1]) ? t + STEP : t, 0);
  const tail = act.bump === undefined ? 0.3 : BUMP * BUMPS;
  const walls = new Set<string>();
  for (let y = 0; y <= 3; y++) {
    walls.add("3," + String(y));
  }
  for (let x = 0; x <= 3; x++) {
    walls.add(String(x) + ",3");
  }
  if (act.far) {
    for (let x = 0; x <= 3; x++) {
      walls.add(String(x) + ",7");
    }
    for (let y = 0; y <= 3; y++) {
      walls.add("11," + String(y));
    }
  }
  // where the player stands u seconds into the walk
  function at(u: number): [number, number] {
    let t = 0;
    for (let i = 0; i + 1 < act.route.length; i++) {
      if (!near(act.route[i], act.route[i + 1])) {
        continue;
      }
      if (u < t + STEP) {
        const f = (u - t) / STEP;
        return [lerp(act.route[i][0], act.route[i + 1][0], f),
          lerp(act.route[i][1], act.route[i + 1][1], f)];
      }
      t += STEP;
    }
    return act.route[act.route.length - 1];
  }
  function tile(x: number, y: number, w: number, h: number, r: number,
    fill: string): void {
    cx.fillStyle = fill;
    cx.beginPath();
    cx.roundRect(x, y, w, h, r);
    cx.fill();
  }
  function draw(u: number): void {
    const v = u - LEAD;
    let [px, py] = act.route[0];
    let hit = "";
    let flag = true;
    let pop = 0;
    if (v >= walk) {
      [px, py] = act.route[act.route.length - 1];
      if (act.bump !== undefined) {
        const tb = (v - walk) / BUMP;
        const g = tb - Math.floor(tb);
        if (tb < BUMPS) {
          const d = g < 0.25 ? g / 0.25 : g < 0.5 ? (0.5 - g) / 0.25 : 0;
          px += 0.3 * d * act.bump[0];
          hit = g > 0.2 && g < 0.4 ? act.bump[1] : "";
        }
      }
      flag = act.win !== true;
      pop = act.pop === undefined ? 0 : ease((v - walk - tail) / 0.3);
    } else if (v > 0) {
      [px, py] = at(v);
    }
    cx.clearRect(0, 0, W, H);
    for (let y = 0; y < GH; y++) {
      for (let x = 0; x < GW; x++) {
        const key = String(x) + "," + String(y);
        if (!walls.has(key)) {
          tile(x * TILE + 1.5, y * TILE + 1.5, TILE - 3, TILE - 3, TILE / 8,
            PAL.floor[(x + y) % 2]);
          continue;
        }
        const lit = hit === key;
        tile(x * TILE + 1.5, y * TILE + 1.5, TILE - 3, TILE - 3, TILE / 8,
          lit ? PAL.hit : PAL.wall);
        tile(x * TILE + TILE / 8, y * TILE + TILE / 8, TILE * 0.75,
          TILE * 0.21, TILE / 19, lit ? PAL.hitcap : PAL.cap);
      }
    }
    cx.save();
    cx.beginPath();
    cx.rect(0, 0, W, H);
    cx.clip();
    const k = TILE / 40;
    if (flag) {
      cx.strokeStyle = PAL.pole;
      cx.lineWidth = 2.5 * k;
      cx.lineCap = "round";
      cx.beginPath();
      cx.moveTo(TILE + 14 * k, TILE + 31 * k);
      cx.lineTo(TILE + 14 * k, TILE + 9 * k);
      cx.stroke();
      cx.fillStyle = PAL.cloth;
      cx.beginPath();
      cx.moveTo(TILE + 15 * k, TILE + 9 * k);
      cx.lineTo(TILE + 31 * k, TILE + 14.5 * k);
      cx.lineTo(TILE + 15 * k, TILE + 20 * k);
      cx.closePath();
      cx.fill();
    }
    const cxp = px * TILE + TILE / 2;
    const cyp = py * TILE + TILE / 2;
    cx.fillStyle = PAL.skin;
    cx.beginPath();
    cx.arc(cxp, cyp, 12.5 * k, 0, Math.PI * 2);
    cx.fill();
    cx.fillStyle = PAL.eye;
    cx.beginPath();
    cx.arc(cxp - 4.5 * k, cyp - 2 * k, 2.2 * k, 0, Math.PI * 2);
    cx.arc(cxp + 4.5 * k, cyp - 2 * k, 2.2 * k, 0, Math.PI * 2);
    cx.fill();
    cx.restore();
    if (pop > 0.5 && act.pop !== undefined) {
      const bw = TILE * 5.6;
      const bh = TILE * 1.5;
      const bx = (W - bw) / 2;
      const by = (H - bh) / 2;
      tile(bx, by, bw, bh, 10 * k, PAL.win);
      cx.strokeStyle = PAL.rim;
      cx.lineWidth = 1.5 * k;
      cx.stroke();
      cx.fillStyle = PAL.winink;
      cx.font = "bold " + String(TILE * 0.6) + "px " + MONO;
      cx.textAlign = "center";
      cx.fillText(act.pop, W / 2, by + bh * 0.66);
    }
  }
  // the frames run to just past the verdict, and the rest is one still
  const still = LEAD + walk + tail + 0.4;
  const shots: [number, number][] = [];
  for (let i = 0; i / FPS < still; i++) {
    shots.push([i / FPS, 1 / FPS]);
  }
  shots.push([still, HOLD]);
  gif_report(gif_write(act.name, cv, draw, shots));
}

// Main
// ====

if (import.meta.main) {
  hero("hero", "#1f2328", "#656d76");
  hero("hero_dark", "#ffffff", "#8b949e");
  parallel();
  for (const act of ACTS) {
    game(act);
  }
}
