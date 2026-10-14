// Inline SVG icon set, cell-grid SVG renderer, theme -> CSS variables.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- shared inline-SVG icon set (tabs + file explorer) -------------------
// One source of truth. Feather/Lucide-style line glyphs, hand-authored (never
// fetched — the CSP blocks remote anything). All monochrome: fill:none +
// stroke:currentColor, so they inherit the element's theme text color (muted
// for inactive rows, --fg/--accent when active, --on-sel when selected). The
// wrapper carries class "ficon"; CSS sizes it (~14px) and aligns it to text.
const ICON_PATHS = {
  // document with a folded corner + a couple of text lines
  doc:      '<path d="M13 3H6a1 1 0 0 0-1 1v16a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V9z"/><path d="M13 3v6h6"/>',
  docLines: '<path d="M13 3H6a1 1 0 0 0-1 1v16a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V9z"/><path d="M13 3v6h6"/><path d="M8.5 13.5h7M8.5 17h4.5"/>',
  // </> code chevrons
  code:     '<path d="M10 8l-4 4 4 4"/><path d="M14 8l4 4-4 4"/>',
  // { } braces (config: json / toml)
  braces:   '<path d="M9 4c-1.6 0-2 .9-2 2v2c0 1.1-.6 2-2 2 1.4 0 2 .9 2 2v2c0 1.1.4 2 2 2"/><path d="M15 4c1.6 0 2 .9 2 2v2c0 1.1.6 2 2 2-1.4 0-2 .9-2 2v2c0 1.1-.4 2-2 2"/>',
  // # hash (stylesheets)
  hash:     '<path d="M9.5 4L7.5 20M16.5 4l-2 16M5 9.5h14M4.5 14.5h14"/>',
  // >_ terminal window (shell scripts)
  term:     '<rect x="3" y="4.5" width="18" height="15" rx="2"/><path d="M7 10l3 2.5-3 2.5M13 15h4"/>',
  // padlock (lockfiles)
  lock:     '<rect x="5" y="11" width="14" height="9" rx="2"/><path d="M8 11V7.5a4 4 0 0 1 8 0V11"/>',
  // closed / open folders
  folder:   '<path d="M3 6.5a2 2 0 0 1 2-2h3.6a1 1 0 0 1 .7.3L11 6.5h6a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/>',
  folderOpen:'<path d="M3 7a2 2 0 0 1 2-2h3.6a1 1 0 0 1 .7.3L11 7h6a2 2 0 0 1 2 2v1H3z"/><path d="M3 10h17.2a1 1 0 0 1 .97 1.24l-1.5 6A1 1 0 0 1 18.7 18H5a2 2 0 0 1-2-2z"/>',
};
// extension → glyph key. Every requested extension is covered; unknown → doc.
const EXT_ICON = {
  rs:'code', ts:'code', tsx:'code', js:'code', jsx:'code', py:'code', go:'code', html:'code',
  json:'braces', toml:'braces',
  css:'hash', scss:'hash',
  sh:'term',
  lock:'lock',
  md:'docLines', txt:'docLines',
};
function iconSvg(key){
  return '<svg class="ficon" viewBox="0 0 24 24" fill="none" stroke="currentColor" '
    + 'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">'
    + (ICON_PATHS[key]||ICON_PATHS.doc) + '</svg>';
}
// Public helper: pick an icon for a name. dir → folder (open when expanded).
function fileIcon(name, opts){
  opts=opts||{};
  if(opts.dir) return iconSvg(opts.expanded?"folderOpen":"folder");
  const m=/\.([A-Za-z0-9]+)$/.exec(String(name||""));
  const ext=m?m[1].toLowerCase():"";
  return iconSvg(EXT_ICON[ext]||"doc");
}

// Vertical box-drawing glyphs the editor uses for the line-number gutter,
// tree guides and rules. Rendered as crisp full-height vector lines instead of
// the font glyph — stacked glyphs leave gaps at our cell height, so the gutter
// looked dashed; a rule per cell is contiguous row-to-row, i.e. one clean line.
// The bridge sends one string per run whose CELLS are already column-aligned
// (a wide glyph is followed by the blank cell it occupies). What JS must not
// do is count UTF-16 units: a ZWJ family, a flag, a skin tone and a combining
// cluster are each ONE cell made of several units, so `.length` pinned their
// pieces to separate columns and the browser could no longer shape them —
// the emoji rendered as a fragment followed by a gap.
//
// `Intl.Segmenter` gives the real cluster boundaries. ASCII-only runs (the
// overwhelming majority, every frame) take a fast path that skips it.
const GRAPHEMES = typeof Intl !== "undefined" && Intl.Segmenter
  ? new Intl.Segmenter(undefined, { granularity: "grapheme" })
  : null;
const ASCII = /^[\x20-\x7E]*$/;
function cellUnits(t){
  if(ASCII.test(t)) return null;                       // fast path: 1 unit = 1 cell
  if(GRAPHEMES) return [...GRAPHEMES.segment(t)].map(g => g.segment);
  return [...t];                                       // last resort: code points
}
// How many cells a run occupies.
function cellLen(t){ const g=cellUnits(t); return g ? g.length : t.length; }

const VRULE = "│┃";
// Draw a block of cells (rows of styled runs) as SVG <text>/<tspan>.
function cellsSvg(cells, wCells){
  let s=`<svg class="cells" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${px(wCells,CW)} ${px(cells.length,CH)}" preserveAspectRatio="xMinYMin slice" font-size="${FONT}">`;
  let rules="";   // vector vertical rules, drawn above the text layer
  for(let row=0; row<cells.length; row++){
    const y=(row+1)*CH - CH*(5/18);   // baseline: 5px up at the base 18px cell, scaled with it
    let col=0;
    for(const r of cells[row]){
      const ebg=r.r?(r.fg||"#d4d4d4"):r.bg;
      if(ebg){ s+=`<rect x="${(px(col,CW)).toFixed(1)}" y="${(row*CH).toFixed(1)}" width="${(px(cellLen(r.t),CW)).toFixed(1)}" height="${CH}" fill="${ebg}"/>`; }
      col+=cellLen(r.t);
    }
    s+=`<text y="${y.toFixed(1)}" xml:space="preserve">`;
    col=0;
    for(const r of cells[row]){
      const fill=r.r?(r.bg||"#1e1e1e"):(r.fg||"#d4d4d4");
      const weight=r.b?` font-weight="bold"`:"";
      const style=r.i?` font-style="italic"`:"";
      const deco=r.u?` text-decoration="underline"`:"";
      // Substitute vertical box-drawing chars with a vector rule: collect a
      // full-height line per cell, and blank the glyph in the text so only the
      // rule shows (no dashed glyph behind it).
      let shown=r.t;
      const vunits=cellUnits(r.t);              // null when one unit is one cell
      if((vunits||r.t).length && (vunits||[...r.t]).some(ch=>VRULE.includes(ch))){
        const list=vunits||[...r.t];
        for(let i=0;i<list.length;i++){ if(VRULE.includes(list[i])){
          const cx=px(col+i,CW)+CW/2;
          const hv=(list[i]==="┃"?1.8:1.1)*zoom;
          rules+=`<rect x="${(cx-hv/2).toFixed(2)}" y="${(row*CH).toFixed(1)}" width="${hv}" height="${CH}" fill="${fill}"/>`;
        }}
        shown=list.map(ch=>VRULE.includes(ch)?" ":ch).join("");
      }
      // Pin EVERY glyph to its exact cell column via a per-character x list.
      // The font's glyph advance isn't exactly CW, so relying on natural advance
      // (or even textLength stretching, which distributes spacing differently
      // for a short highlighted run than for the long run it was part of) lets
      // glyphs drift when run boundaries change — e.g. toggling occurrence /
      // current-line highlights re-split runs and visibly nudged the text. A
      // hard x per cell makes column position independent of run grouping.
      //
      // One x per CELL, never per UTF-16 unit — and an x on a character starts
      // a new SVG text chunk, across which the browser will not shape. So a
      // cluster made of several units (a ZWJ sequence, a flag, a skin tone, a
      // base + combining mark) gets ONE x on its own tspan and no x inside it;
      // single-unit cells stay batched into one tspan with an x list.
      const attrs=`fill="${fill}"${weight}${style}${deco}`;
      const units=cellUnits(shown);            // null when one unit is one cell
      if(!units){
        let xs="";
        for(let i=0;i<shown.length;i++){ xs+=(i?" ":"")+px(col+i,CW).toFixed(1); }
        s+=`<tspan x="${xs}" ${attrs}>${esc(shown)}</tspan>`;
        col+=shown.length;
      }else{
        for(let i=0;i<units.length;){
          if(units[i].length===1){             // a plain BMP character
            let j=i,txt="",xs="";
            while(j<units.length && units[j].length===1){
              txt+=units[j]; xs+=(j>i?" ":"")+px(col+j,CW).toFixed(1); j++;
            }
            s+=`<tspan x="${xs}" ${attrs}>${esc(txt)}</tspan>`;
            i=j;
          }else{
            s+=`<tspan x="${px(col+i,CW).toFixed(1)}" ${attrs}>${esc(units[i])}</tspan>`;
            i++;
          }
        }
        col+=units.length;
      }
    }
    s+=`</text>`;
  }
  s+=rules;
  s+=`</svg>`;
  return s;
}

// Parse `#rrggbb` into [r,g,b], or null for anything else (a theme may leave a
// colour at terminal "reset").
function hexRgb(h){ const m=/^#?([0-9a-fA-F]{6})$/.exec((h||"").trim()); if(!m) return null;
  const n=parseInt(m[1],16); return [(n>>16)&255,(n>>8)&255,n&255]; }
function relLum(c){ const f=v=>{v/=255; return v<=.03928?v/12.92:Math.pow((v+.055)/1.055,2.4);};
  return .2126*f(c[0])+.7152*f(c[1])+.0722*f(c[2]); }
function contrast(a,b){ const l1=relLum(a),l2=relLum(b); const hi=Math.max(l1,l2),lo=Math.min(l1,l2);
  return (hi+.05)/(lo+.05); }
// Blend two `#rrggbb` colours, `k` of the way from `a` to `b`. Null when
// either is unparseable (a theme may leave a colour at terminal reset), so
// the caller can fall back to the stylesheet default.
function mix(a,b,k){
  const ca=hexRgb(a), cb=hexRgb(b);
  if(!ca||!cb) return null;
  return "#"+ca.map((v,i)=>Math.round(v*(1-k)+cb[i]*k).toString(16).padStart(2,"0")).join("");
}

// Text for a control painted IN a theme colour (a row filled with the
// menu-highlight, a button filled with the accent). The theme names the fill
// and never the ink on it, and themes are user data, so the ink is chosen
// here. Chosen by CONTRAST rather than a luminance cutoff: a mid-light blue
// (tokyo-night's #7aa2f7) sits below any cutoff that keeps white on the dark
// accents, and white on it measures 2.5:1 — comparing both candidates picks
// the readable one at every luminance.
function onColor(hex){ const c=hexRgb(hex); if(!c) return null;
  const dark="#10131a", light="#ffffff";
  return contrast(c,hexRgb(dark))>=contrast(c,hexRgb(light))?dark:light; }

// Blend `muted` toward `fg` in tenths until it clears the 3:1 floor against
// every surface it is drawn on. Returns the original string when it already
// does (or when anything is unparseable, so a terminal-reset colour keeps the
// stylesheet default).
function legibleMuted(muted,fg,surfaces){
  const m=hexRgb(muted), f=hexRgb(fg);
  const grounds=(surfaces||[]).map(hexRgb).filter(Boolean);
  if(!m||!f||!grounds.length) return muted;
  const ok=c=>grounds.every(g=>contrast(c,g)>=3);
  if(ok(m)) return muted;
  for(let t=1;t<=10;t++){
    const c=m.map((v,i)=>Math.round(v+(f[i]-v)*t/10));
    if(ok(c)) return "#"+c.map(v=>v.toString(16).padStart(2,"0")).join("");
  }
  // Nothing short of the foreground clears every ground; say so rather than
  // returning a blend that still fails on one of them.
  return fg;
}

// Seed the chrome CSS variables from the editor's active theme so the native
// HTML matches the terminal palette (instead of a fixed dark scheme). Each var
// falls back to its :root default when the theme leaves a color at terminal
// "reset" (null).
function applyTheme(t){
  if(!t) return;
  const r=document.documentElement.style;
  const set=(k,v)=>{ if(v) r.setProperty(k,v); else r.removeProperty(k); };
  set("--bg",t.bg); set("--fg",t.fg); set("--accent",t.accent);
  // `--muted` is chrome SECONDARY TEXT (tab names, status segments, setting
  // descriptions), seeded from the theme's gutter grey. A gutter grey is
  // chosen to recede behind code, not to be read as a label: tokyo-night's
  // #565f89 lands at 2.5:1 on the panel surface, which took every muted string
  // in the chrome under the 3:1 floor. Lift it toward the foreground until it
  // clears that floor on both surfaces it is drawn on; a theme already above
  // the floor is left exactly as it is.
  // Every ground muted text is drawn on: panel surfaces (`--bg2`), the buffer
  // (`--bg`) and the menu/dropdown surface (`--bg3`) — a menu accelerator is
  // muted text on the last one, and leaving it out let the floor report
  // success at 2.18:1 (dark) and 1.90:1 (nostalgia).
  set("--muted",legibleMuted(t.muted,t.fg,[t.popupBg,t.bg]));
  // The menu surface can be a different world from the editor: nostalgia
  // paints menus light-grey with black text over a blue buffer with yellow
  // text, so no single muted value serves both grounds — blending the editor
  // one toward the editor foreground reached 2.18:1 on the menu. Muted text
  // drawn on a menu derives from the MENU's own foreground instead.
  set("--muted-menu",legibleMuted(t.muted,t.menuFg,[t.menuBg]));
  set("--bg2",t.popupBg); set("--bg3",t.menuBg);
  set("--menuhi",t.menuHi); set("--border",t.border);
  // The EDITOR's own popup ground, kept separate from `--bg2`/`--fg` because a
  // web theme re-skins those: a popup body that carries editor ink (an LSP
  // hover's highlighted code, the theme inspector) has to stand on the ground
  // that ink was chosen against, or a light chrome skin puts dark-theme text
  // on a light card. No web theme overrides these two, by design.
  set("--ed-popup-bg",t.popupBg); set("--ed-popup-fg",t.popupFg);
  set("--status-bg",t.statusBg); set("--status-fg",t.statusFg);
  set("--on-accent", onColor(t.accent));
  // Same question for a row filled SOLID with the menu-highlight: a theme
  // whose highlight is light (gruvbox's amber) left hardcoded white text at
  // 1.7:1. Rows that merely *tint* their surface keep `--on-sel` below.
  // The theme's own ink for its highlight fill wins; `onColor` is the
  // fallback for a theme that leaves it at terminal reset.
  set("--on-menuhi", t.menuHighlightFg || onColor(t.menuHi));
  // Selected rows are a translucent --ui-accent tint over the panel surface
  // (not a solid menuHi fill), so the readable text colour on them is simply
  // the theme foreground.
  set("--on-sel", t.fg);
  // Frame-chrome surface: pull the buffer bg toward deep navy. Dark themes get
  // the design's near-black navy shell; light themes only a gently shaded
  // frame (same hue pull, ratio picked by bg luminance). Null bg (terminal
  // reset) falls back to the :root color-mix default.
  const darkBg = onColor(t.bg)==="#ffffff";
  set("--shell", mix(t.bg, "#060b13", darkBg?0.45:0.08));
}

// Selectors for natively-scrolled chrome whose scrollTop must survive a
// rebuild (otherwise a re-render would snap them back to the top). With
// per-region patching the snapshot/restore is scoped to the one region
// container actually being rebuilt — untouched regions keep their scroll
// positions by simply not being touched.
// `.w-tree` is the orchestrator dock's session-list scroll container (the
// list is rendered in full and scrolls natively); it must survive the dock's
// frequent re-renders or the list snaps to the top mid-scroll.
const SCROLL_KEEP = ".set-items,.set-cats,.w-list,.w-tree,.widget-surface";

