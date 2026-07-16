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
      if(ebg){ s+=`<rect x="${(px(col,CW)).toFixed(1)}" y="${(row*CH).toFixed(1)}" width="${(px(r.t.length,CW)).toFixed(1)}" height="${CH}" fill="${ebg}"/>`; }
      col+=r.t.length;
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
      if([...r.t].some(ch=>VRULE.includes(ch))){
        for(let i=0;i<r.t.length;i++){ if(VRULE.includes(r.t[i])){
          const cx=px(col+i,CW)+CW/2;
          const hv=(r.t[i]==="┃"?1.8:1.1)*zoom;
          rules+=`<rect x="${(cx-hv/2).toFixed(2)}" y="${(row*CH).toFixed(1)}" width="${hv}" height="${CH}" fill="${fill}"/>`;
        }}
        shown=[...r.t].map(ch=>VRULE.includes(ch)?" ":ch).join("");
      }
      // Pin EVERY glyph to its exact cell column via a per-character x list.
      // The font's glyph advance isn't exactly CW, so relying on natural advance
      // (or even textLength stretching, which distributes spacing differently
      // for a short highlighted run than for the long run it was part of) lets
      // glyphs drift when run boundaries change — e.g. toggling occurrence /
      // current-line highlights re-split runs and visibly nudged the text. A
      // hard x per cell makes column position independent of run grouping.
      let xs="";
      for(let i=0;i<shown.length;i++){ xs+=(i?" ":"")+px(col+i,CW).toFixed(1); }
      s+=`<tspan x="${xs}" fill="${fill}"${weight}${style}${deco}>${esc(shown)}</tspan>`;
      col+=r.t.length;
    }
    s+=`</text>`;
  }
  s+=rules;
  s+=`</svg>`;
  return s;
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
  set("--muted",t.muted); set("--bg2",t.popupBg); set("--bg3",t.menuBg);
  set("--menuhi",t.menuHi); set("--border",t.border);
  set("--status-bg",t.statusBg); set("--status-fg",t.statusFg);
  // Pick black/white text for fills painted in the accent / menu-highlight color
  // by luminance, so a control filled with a light accent (e.g. high-contrast's
  // white cursor) still shows its label instead of white-on-white.
  const onColor=hex=>{ const m=/^#?([0-9a-fA-F]{6})$/.exec((hex||"").trim()); if(!m) return null;
    const n=parseInt(m[1],16), lin=c=>{c/=255; return c<=.03928?c/12.92:Math.pow((c+.055)/1.055,2.4);};
    const L=.2126*lin((n>>16)&255)+.7152*lin((n>>8)&255)+.0722*lin(n&255);
    return L>.45?"#10131a":"#ffffff"; };
  set("--on-accent", onColor(t.accent));
  // Selected rows are a translucent --ui-accent tint over the panel surface
  // (not a solid menuHi fill), so the readable text colour on them is simply
  // the theme foreground.
  set("--on-sel", t.fg);
  // Frame-chrome surface: pull the buffer bg toward deep navy. Dark themes get
  // the design's near-black navy shell; light themes only a gently shaded
  // frame (same hue pull, ratio picked by bg luminance). Null bg (terminal
  // reset) falls back to the :root color-mix default.
  const mix=(a,b,k)=>{ const pa=/^#?([0-9a-fA-F]{6})$/.exec((a||"").trim()),
      pb=/^#?([0-9a-fA-F]{6})$/.exec((b||"").trim());
    if(!pa||!pb) return null;
    const na=parseInt(pa[1],16), nb=parseInt(pb[1],16);
    const ch=s=>Math.round(((na>>s)&255)*(1-k)+((nb>>s)&255)*k);
    return "#"+[16,8,0].map(s=>ch(s).toString(16).padStart(2,"0")).join(""); };
  const darkBg = onColor(t.bg)==="#ffffff";
  set("--shell", mix(t.bg, "#060b13", darkBg?0.45:0.08));
}

// Selectors for natively-scrolled chrome whose scrollTop must survive a
// rebuild (otherwise a re-render would snap them back to the top). With
// per-region patching the snapshot/restore is scoped to the one region
// container actually being rebuilt — untouched regions keep their scroll
// positions by simply not being touched.
const SCROLL_KEEP = ".set-items,.set-cats,.w-list,.widget-surface";

