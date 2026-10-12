// Winning Is Impossible: the visual half. The game itself is main.bend,
// imported as a module: the bend loader compiles it on the fly, and every def
// becomes a function on Game. The map is asked from Game.grid, so the level has
// one author. Every move goes through Game.replay; this file only draws the
// state that comes back.

import Game from "../main.bend";

// Map
// ===

const ROWS  = Game.grid(Game.map_h() - 1).split("\n");
const H     = ROWS.length;
const W     = ROWS[0].length;
const WALLS = new Set();
let FLAG  = { x: 0, y: 0 };
for (let y = 0; y < H; y++) {
  for (let x = 0; x < W; x++) {
    const c = ROWS[y][x];
    if (c === "#") WALLS.add(x + "," + y);
    if (c === "F") FLAG = { x, y };
  }
}

// Bend bridge
// ===========
// The conventions: a constructor is {$: "Name", field: value, ...} (live
// fields only), Nat is a number, Bool is a plain boolean. Stepping onto the
// flag's cell is the win: the game sets `won` itself.

const one  = (act) => ({ $: "Con", head: { $: act }, tail: { $: "Nil" } });
const send = (act, g) => Game.replay(g, one(act));

let st    = Game.start();
let moves = 0;

// Render
// ======

const TILE = 44;
const cv   = document.getElementById("game");
const cx   = cv.getContext("2d");
cv.width  = W * TILE;
cv.height = H * TILE;
cv.style.width = Math.min(W * TILE, 616) + "px";

let anim = null; // {fx, fy, tx, ty, t0}
let bump = null; // {dx, dy, t0}

function drawFlag(px, py, t) {
  const wob = Math.sin(t / 300) * 2;
  cx.save();
  cx.translate(px + TILE / 2, py + TILE / 2);
  cx.shadowColor = "#ffd25e";
  cx.shadowBlur  = 14;
  cx.strokeStyle = "#8a7440";
  cx.lineWidth   = 2;
  cx.beginPath();
  cx.moveTo(-6, 12);
  cx.lineTo(-6, -14);
  cx.stroke();
  cx.fillStyle = "#ffd25e";
  cx.beginPath();
  cx.moveTo(-5, -14);
  cx.lineTo(13 + wob, -9);
  cx.lineTo(-5, -3);
  cx.closePath();
  cx.fill();
  cx.restore();
}

function drawPlayer(px, py, t) {
  const br = (bump && t - bump.t0 < 120) ? (1 - (t - bump.t0) / 120) * 6 : 0;
  const bx = bump ? bump.dx * br : 0;
  const by = bump ? bump.dy * br : 0;
  cx.save();
  cx.translate(px + TILE / 2 + bx, py + TILE / 2 + by);
  cx.shadowColor = "#59e3ff";
  cx.shadowBlur  = 16;
  cx.fillStyle   = "#59e3ff";
  cx.beginPath();
  cx.arc(0, 0, TILE * 0.3, 0, Math.PI * 2);
  cx.fill();
  cx.shadowBlur = 0;
  cx.fillStyle  = "#0b0e14";
  cx.beginPath();
  cx.arc(-4, -3, 2.4, 0, Math.PI * 2);
  cx.arc(4, -3, 2.4, 0, Math.PI * 2);
  cx.fill();
  cx.restore();
}

function draw(t) {
  cx.clearRect(0, 0, cv.width, cv.height);
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      const px = x * TILE;
      const py = y * TILE;
      if (WALLS.has(x + "," + y)) {
        cx.fillStyle = "#2a3347";
        cx.fillRect(px + 1, py + 1, TILE - 2, TILE - 2);
        cx.fillStyle = "#39445e";
        cx.fillRect(px + 4, py + 4, TILE - 8, TILE / 2 - 6);
      } else {
        cx.fillStyle = (x + y) % 2 ? "#131826" : "#11151f";
        cx.fillRect(px, py, TILE, TILE);
      }
    }
  }
  drawFlag(FLAG.x * TILE, FLAG.y * TILE, t);
  let x = Number(st.x);
  let y = Number(st.y);
  if (anim) {
    const k = Math.min((t - anim.t0) / 90, 1);
    x = anim.fx + (anim.tx - anim.fx) * k;
    y = anim.fy + (anim.ty - anim.fy) * k;
    if (k === 1) anim = null;
  }
  drawPlayer(x * TILE, y * TILE, t);
  requestAnimationFrame(draw);
}
requestAnimationFrame(draw);

// Input
// =====

const toastEl = document.getElementById("toast");
let toastTimer = null;

function toast(msg) {
  toastEl.textContent = msg;
  toastEl.style.opacity = 1;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { toastEl.style.opacity = 0; }, 1200);
}

const DELTA = { Up: [0, -1], Down: [0, 1], Left: [-1, 0], Right: [1, 0] };

function act(name) {
  const fx = Number(st.x);
  const fy = Number(st.y);
  st = send(name, st);
  const tx = Number(st.x);
  const ty = Number(st.y);
  moves++;
  document.getElementById("moves").textContent = moves;
  const [dx, dy] = DELTA[name];
  if (tx === fx && ty === fy) {
    bump = { dx, dy, t0: performance.now() };            // a wall said no
  } else if (Math.abs(tx - fx) <= 1 && Math.abs(ty - fy) <= 1) {
    anim = { fx, fy, tx, ty, t0: performance.now() };    // a plain step
  }                                                      // else: the wrap
  if (st.won) {
    document.getElementById("status").textContent = "YOU WON?!";
    toast("YOU WON?! please file a bug: this is mathematically impossible");
  }
}

const KEYS = {
  ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right",
  w: "Up", s: "Down", a: "Left", d: "Right",
};

document.addEventListener("keydown", (e) => {
  const name = KEYS[e.key];
  if (!name) return;
  e.preventDefault();
  act(name);
});

for (const b of document.querySelectorAll(".pad button")) {
  b.addEventListener("click", () => act(b.dataset.act));
}
