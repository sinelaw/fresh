// Metrics (cell<->px), zoom, COSMOS shell geometry, DOM helpers.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
"use strict";
// --------------------------------------------------------------------------
// Real-render client. NO local model and NO re-implemented layout: the server
// runs the real Editor::render and reports the cell grid for buffer interiors
// plus the *semantic* chrome model (menu tree, tabs, status segments) the
// pipeline already computed. Buffer interiors are drawn as cells; chrome is
// drawn as native HTML. Every chrome interaction is sent back to the real
// Editor::handle_mouse at the cell coords the pipeline reported.
// Transport: one WebSocket — the server pushes region-diff frames whenever
// the scene changes (no client polling), and all input rides the same socket.
// --------------------------------------------------------------------------
let scene = null;
// ---- typography metrics: measured, not constant (docs §3.3) ----------------
// The cell size is derived from a canvas measureText of the SAME monospace
// stack the SVG renders with, at the current (zoomed) font size — not from a
// hardcoded constant. CW = measured glyph advance × CELL_AIR, where CELL_AIR
// is the shipped grid's cell/advance ratio (8.2px cells over the 7.83px
// advance the original hardcoded CW was tuned against), so zoom 1.0 renders
// pixel-identical to the old constants while a differently-resolved font (or
// a future user font setting) still re-derives the whole grid from its own
// measurement. CH keeps the shipped 18/13 line-height:font-size ratio. All
// cell↔pixel mapping (place/px/cellAt/cellsSvg/caret/resize) reads these
// lets, so one measureMetrics() + render() + resize() retunes the entire UI.
// `zoom` (Ctrl+= / Ctrl+- / Ctrl+0, Ctrl+wheel — frontend-owned, see the
// keydown handler) multiplies the base font size; it is a pure view
// preference persisted in localStorage (same class as mShowMods), never
// editor state — the editor only sees the resulting cols/rows re-fit.
// Family and base size come from the CSS custom properties (--font-family /
// --font-size-base) so the stylesheet stays the single place typography is
// declared: the same family and size the chrome inherits are what the cell grid
// is measured with.
const rootCss = getComputedStyle(document.documentElement);
const FONT_STACK = rootCss.getPropertyValue("--font-family").trim()
  || "ui-monospace,SFMono-Regular,JetBrains Mono,Menlo,Consolas,monospace";
const BASE_FONT = parseFloat(rootCss.getPropertyValue("--font-size-base")) || 13;  // CSS px at zoom 1.0
const CELL_AIR = 1.0477;          // cell width / glyph advance (see above)
let zoom = 1;
try{ const z=parseFloat(localStorage.getItem("fresh.zoom")); if(z>=0.5&&z<=3) zoom=z; }catch(_){}
let FONT = BASE_FONT, CW = 8.2, CH = 18;   // refined by measureMetrics()
const measureCtx = document.createElement("canvas").getContext("2d");
function measureMetrics(){
  FONT = Math.round(BASE_FONT*zoom*100)/100;
  measureCtx.font = FONT+"px "+FONT_STACK;
  const adv = measureCtx.measureText("M0".repeat(60)).width/120;  // per-cell advance
  CW = adv>0 ? Math.round(adv*CELL_AIR*100)/100 : 8.2*zoom;       // canvas-less fallback
  CH = Math.round(FONT*(18/13)*100)/100;
  // Publish the zoomed size back to CSS: chrome sizes itself in em off
  // --font-size, so the native UI scales with the buffer instead of staying
  // pinned at the unzoomed base.
  document.documentElement.style.setProperty("--font-size", FONT+"px");
  // Font/cell alignment: the SVG buffer pins every glyph to a CW-pitch column,
  // but native chrome text flows at the font's *natural* advance (~CW/CELL_AIR),
  // so N characters of chrome were narrower than the N cells the TUI budgeted
  // for them — which is why chrome boxes sized to a server cell-rect couldn't
  // hold their own text. Add exactly the missing tracking (CW − advance) as
  // letter-spacing so one chrome glyph advances one cell, matching the grid.
  const advNat = adv>0 ? adv : CW/CELL_AIR;
  document.documentElement.style.setProperty("--cell-tracking", (Math.round((CW-advNat)*1000)/1000)+"px");
}
measureMetrics();

// ---- COSMOS shell geometry ------------------------------------------------
// The grid is inset from the window (wallpaper margin + device bezel, see the
// #app rule); SHELL mirrors the CSS vars so JS can position the bezel and
// inset the dock card. APPX/APPY is #app's live viewport origin — every
// pixel→cell conversion (cellAt, border drags) maps through it.
const SHELL=(()=>{ const n=k=>parseFloat(rootCss.getPropertyValue(k))||0;
  return { pad:n("--shell-pad"), top:n("--bezel-top"), bot:n("--bezel-bot"),
           side:n("--bezel-side"), gap:n("--dock-gap") }; })();
let APPX=0, APPY=0;
function syncAppOrigin(){ const r=document.getElementById("app").getBoundingClientRect(); APPX=r.left; APPY=r.top; }
const appH=()=>document.getElementById("app").clientHeight;
// Pixel width of a full-height LEFT dock (0 when the dock is hidden/absent):
// the bezel's screen starts to its right, so the sidebar floats free of it.
function dockWidthPx(){
  if(!scene||!scene.regions) return 0;
  for(const s of (scene.regions.widgets||[]))
    if(s.kind==="dock"&&s.rect&&s.rect.x===0&&s.rect.w<(scene.w||0)) return px(s.rect.w,CW);
  return 0;
}
// Wrap the bezel around the grid minus the dock columns. Runs after every
// render/frame (dock width and grid size are server-driven) and on window
// resize; on mobile (or before the first scene) the bezel is hidden.
function layoutShell(){
  syncAppOrigin();
  const dev=document.getElementById("device");
  if(!dev) return;
  if(isMobile()||!scene){ dev.classList.remove("on"); return; }
  dev.classList.add("on");
  const dw=dockWidthPx();
  // Clip full-grid-width chrome rows (menubar) to the screen — see the CSS.
  document.documentElement.style.setProperty("--clip-left", dw+"px");
  dev.style.left=(APPX+dw-SHELL.side)+"px";
  dev.style.top=(APPY-SHELL.top)+"px";
  dev.style.width=(px(scene.w||0,CW)-dw+2*SHELL.side)+"px";
  dev.style.height=(px(scene.h||0,CH)+SHELL.top+SHELL.bot)+"px";
}

function setZoom(z){
  z = Math.round(Math.min(3,Math.max(0.5,z))*100)/100;
  if(z===zoom) return;
  zoom = z;
  try{ localStorage.setItem("fresh.zoom",String(z)); }catch(_){}
  measureMetrics();
  if(scene) render();   // re-place everything at the new metrics now…
  resize();             // …then the editor re-fits cols/rows to the new cell size
}
// Command-palette placement — a pure frontend view preference (same class as
// `zoom` / `mShowMods`), persisted in localStorage, never editor state.
// `true`  → float the palette as a centered modal over the whole view (a
//           `.modal-scrim` dims the dock / explorer / buffers behind it);
// `false` → keep the terminal-style sheet hugging the bottom of the grid.
// The editor is unaffected either way: row clicks/scroll still route through
// its logical suggestion cell rect, so only the pixels move. Toggle with the
// frontend-owned Ctrl/Cmd+Alt+P chord or the mobile ⋮ sheet.
let paletteCentered = true;
try{ if(localStorage.getItem("fresh.palette.centered")==="0") paletteCentered=false; }catch(_){}
function setPaletteCentered(on){
  on=!!on;
  if(on===paletteCentered) return;
  paletteCentered=on;
  try{ localStorage.setItem("fresh.palette.centered", on?"1":"0"); }catch(_){}
  if(scene) render();
}
// Browser zoom or a monitor move changes devicePixelRatio without any event;
// the matchMedia re-arm pattern fires once per change — re-measure (rounding
// of the advance can shift with rasterization) and re-fit the grid.
(function armDpr(){
  matchMedia(`(resolution: ${devicePixelRatio}dppx)`)
    .addEventListener("change",()=>{ measureMetrics(); if(scene) render(); resize(); armDpr(); },{once:true});
})();

const esc = s => String(s).replace(/[&<>"']/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const px = (cells, sz) => (cells*sz);
function place(el, r){ el.style.left=px(r.x,CW)+"px"; el.style.top=px(r.y,CH)+"px"; el.style.width=px(r.w,CW)+"px"; el.style.height=px(r.h,CH)+"px"; }
function div(cls){ const d=document.createElement("div"); d.className=cls; return d; }
// cell coordinate to forward to the editor for a click on a chrome rect
const rectCell = r => ({ col: r.x + Math.floor(r.w/2), row: r.y });

