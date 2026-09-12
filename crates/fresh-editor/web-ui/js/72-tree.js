// The plugin panels, folded from the display list.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
//
// **The web consumes the display list.** A plugin panel — the dock column, a
// floating panel, a sidebar section a plugin mounted — is nodes in the same
// `fresh-ui` tree the terminal folds into cells. The server ships the *items*
// that tree produced for those subtrees (`regions.tree`: rectangle, clip,
// resolved colours, what to draw) and this folds them into DOM the way the
// terminal folds them into cells. Nothing here knows what a widget is: there
// is no spec to lay out, no recorded hit list, no index to echo back.
//
// Input needs nothing of its own. Every element built here lands under the
// document-level mouse handlers (80-input.js), which map the pixel to the
// editor's cell and send it; the server routes that cell over the tree exactly
// as it routes a terminal click — a press on a row selects it, a press on a
// text field places the caret by byte, a wheel scrolls the viewport under it.
// The layer is deliberately NOT a chrome surface for `onChrome`: chrome
// forwards clicks at rectangles it recorded, and this has none to record —
// the cell under the pointer IS the answer, because the items sit exactly
// where the tree laid them out.
// The item's paint, and the QUESTION behind it. The fold answers with two
// colours; the item's `ThemeKey` says which two theme keys they came from, and
// the server ships both (`fgKey`/`bgKey`, see `view/scene.rs`). The answer
// goes on as `--fold-bg` / `--fold-fg` rather than as inline `background` and
// `color`, and the names go on as `data-fg` / `data-bg` — so a *web theme*,
// which is a chrome look layered over the editor's palette and not the palette
// itself, can re-map the handful of keys it dresses in ordinary CSS and
// inherit the fold's answer for everything else. Inline paint could only have
// been overridden with `!important`, which is why the dock wore the terminal's
// colours in every web theme.
//
// **The answer is the FALLBACK, not the value.** These go on inline, and an
// inline custom property beats a stylesheet one exactly as an inline colour
// does — so a theme setting `--fold-bg` would lose the same argument in a new
// spelling. The stylesheet reads `var(--ink-bg, var(--fold-bg, ...))`
// (45-tree.css): a theme states `--ink-bg`, which nothing sets inline, and the
// fold's answer stands wherever it does not.
function itemPaint(el,it){
  if(it.bg) el.style.setProperty("--fold-bg",it.bg);
  if(it.fg) el.style.setProperty("--fold-fg",it.fg);
  if(it.fgKey) el.dataset.fg=it.fgKey;
  if(it.bgKey) el.dataset.bg=it.bgKey;
}
function treeItemEl(it,kindOf){
  const el=div("tree-item k-"+it.kind);
  el.dataset.surface=it.surface;
  // Which surface it belongs to, by name. An item is a SIBLING of its
  // surface marker on the layer (they are both placed at absolute cell
  // rects), so no descendant selector can say "an item of the dock" — and a
  // web theme dressing the dock has to be able to.
  if(kindOf) el.dataset.surfaceKind=kindOf;
  // Painted by a LAYER the surface raised — a pop-over, a context menu — not
  // by its own flow. A theme's ground rules are about the surface's flow ("the
  // dock's body is the well"); a pop-over over that body has to be opaque or
  // it is unreadable, so it must be able to opt out of them by selector.
  if(it.layer) el.dataset.layer="";
  // What the item IS, as opposed to how the fold painted it. Read it with
  // `k-<kind>`: a classed node emits a `fill` over its own rect and its label
  // arrives as a separate `lines` item wearing the same class, so
  // `.k-fill[data-class~="button"]` is the control's box and
  // `.k-lines[data-class~="button"]` is what it says. Matching the class alone
  // would draw the control twice.
  if(it.classes) el.dataset.class=it.classes;
  if(it.key) el.dataset.key=it.key;
  el.style.left=px(it.x,CW)+"px"; el.style.top=px(it.y,CH)+"px";
  el.style.width=px(it.w,CW)+"px"; el.style.height=px(it.h,CH)+"px";
  itemPaint(el,it);
  switch(it.kind){
    case "fill":
      break;
    case "wash":
      // A ground laid over what is under it, keeping the text there: the
      // TUI recolours the cells' backgrounds; here the ground is translucent.
      el.style.pointerEvents="none";
      break;
    case "border":
      if(it.border==="rounded") el.classList.add("rounded");
      break;
    case "rule": {
      // A ground made of one cluster, tiled across the rect the library
      // settled on — the description never said how wide. The glyph rides
      // in `lines[0]`, and this backend is free to ignore it: a horizontal
      // box-drawing rule becomes a vector line for the same reason VRULE
      // does (the font's glyph leaves gaps at our cell metrics), and any
      // other cluster is tiled as text, one per cell.
      const g=(it.lines&&it.lines[0])||"─";
      const hrule="─━═".includes(g);
      for(let r=0;r<it.h;r++){
        const top=px(r,CH);
        if(hrule){
          const hv=(g==="━"?1.8:1.1)*zoom;
          const rule=div("tree-rule");
          rule.style.left="0"; rule.style.top=(top+CH/2-hv/2)+"px";
          rule.style.width=px(it.w,CW)+"px"; rule.style.height=hv+"px";
          el.appendChild(rule);
        }else{
          const row=div("tree-row");
          row.style.top=top+"px";
          row.textContent=g.repeat(Math.max(0,Math.floor(it.w)));
          el.appendChild(row);
        }
      }
      break;
    }
    case "scrim":
      el.classList.add(it.dim?"dim":"opaque");
      break;
    case "lines": {
      if(it.bold) el.style.fontWeight="bold";
      if(it.italic) el.style.fontStyle="italic";
      if(it.underline) el.style.textDecoration="underline";
      // The item's own origin can sit left of / above the visible rect when
      // an enclosing clip cut it; rows are placed from that origin and the
      // element's overflow does the cutting.
      const dx=px(it.ox-it.x,CW), dy=px(it.oy-it.y,CH);
      (it.lines||[]).forEach((line,i)=>{
        const top=dy+px(i,CH);
        if(top+CH<=0||top>=px(it.h,CH)) return;
        // Vertical box-drawing glyphs become vector rules, exactly as the cell
        // SVG does them (VRULE, 20-cells.js): stacked at our cell height the
        // font's own glyph leaves a gap between rows, so a wall a row tall per
        // cell — the dock's divider is one such per row — came out dashed. The
        // rule takes the run's colour from the item (`currentColor`) and the
        // glyph is blanked so nothing shows behind it.
        let shown=line;
        if([...line].some(ch=>VRULE.includes(ch))){
          for(let c=0;c<line.length;c++){
            if(!VRULE.includes(line[c])) continue;
            const hv=(line[c]==="┃"?1.8:1.1)*zoom;
            const rule=div("tree-rule");
            rule.style.left=(dx+px(c,CW)+CW/2-hv/2)+"px"; rule.style.top=top+"px";
            rule.style.width=hv+"px"; rule.style.height=CH+"px";
            el.appendChild(rule);
          }
          shown=[...line].map(ch=>VRULE.includes(ch)?" ":ch).join("");
        }
        const row=div("tree-line"); row.textContent=shown;
        row.style.left=dx+"px"; row.style.top=top+"px";
        row.style.lineHeight=CH+"px"; row.style.height=CH+"px";
        el.appendChild(row);
      });
      break;
    }
    case "scrollbar": {
      const th=div("tree-thumb");
      const [top,len]=it.thumb||[0,0];
      if(it.horizontal){
        th.style.left=px(top,CW)+"px"; th.style.width=px(len,CW)+"px";
        th.style.top="0px"; th.style.height=CH+"px";
      } else {
        th.style.top=px(top,CH)+"px"; th.style.height=px(len,CH)+"px";
      }
      el.appendChild(th);
      // Marks on the track: a half-width bar in the mark's colour over the
      // thumb or track (the TUI's half-block glyph), or the whole cell.
      for(const m of (it.marks||[])){
        const mk=div(m.full?"tree-mark-full":"tree-mark");
        if(it.horizontal){ mk.style.left=px(m.at,CW)+"px"; mk.style.width=CW+"px"; mk.style.top="0px"; mk.style.height=CH+"px"; }
        else { mk.style.top=px(m.at,CH)+"px"; mk.style.height=CH+"px"; }
        mk.style.background=m.color;
        el.appendChild(mk);
      }
      break;
    }
    case "selectable":
      el.style.pointerEvents="none";
      break;
    default: // "host": an embedded window; the web has no cells for it here.
      break;
  }
  return el;
}

// One layer over the whole grid: the surfaces' outlines (markers for tests and
// for the layout animations — they take no input), then every item in paint
// order, then the caret the display list placed inside a surface.
function treeEls(t){
  const out=[];
  if(!t||!t.surfaces||!t.surfaces.length) return out;
  const layer=div("tree");
  layer.style.width=px(scene.w||0,CW)+"px"; layer.style.height=px(scene.h||0,CH)+"px";
  t.surfaces.forEach((s,i)=>{
    // A centered floating panel is modal: the tree blocks the pointer behind
    // it, and the terminal dims what it covers. The dim is drawn here from
    // that fact until the tree declares the scrim itself — it cannot while
    // the fold runs in two bands (retained-mode-ui.md, "Back to a frame").
    if(s.kind==="floating"&&!s.anchored){
      const scrim=div("tree-scrim");
      scrim.style.width=px(scene.w||0,CW)+"px"; scrim.style.height=px(scene.h||0,CH)+"px";
      layer.appendChild(scrim);
    }
    const sd=div("tree-surface "+s.kind+(s.anchored?" anchored":""));
    sd.dataset.index=i;
    if(s.section!==undefined) sd.dataset.section=s.section;
    place(sd,{x:s.x,y:s.y,w:s.w,h:s.h});
    layer.appendChild(sd);
    // A full-height left dock is resized by dragging its last column — the
    // cell the tree draws its wall into. The document's cell mapping already
    // forwards the drag (the layer is not `onChrome`), so this adds only the
    // affordance: the col-resize cursor over the one column that has it.
    if(s.kind==="dock"&&s.x===0&&!isMobile()) layer.appendChild(borderDragHandle(s.x+s.w-1,s.y,s.h));
  });
  const kindOf=i=>(t.surfaces[i]||{}).kind;
  for(const it of (t.items||[])) layer.appendChild(treeItemEl(it,kindOf(it.surface)));
  if(t.cursor){
    const c=div("tree-caret");
    c.style.left=px(t.cursor[0],CW)+"px"; c.style.top=px(t.cursor[1],CH)+"px";
    c.style.width=CW+"px"; c.style.height=CH+"px";
    layer.appendChild(c);
  }
  out.push(layer);
  return out;
}
// The dock's width in cells when a full-height left dock is up, else 0 — read
// off the display list's surfaces, the one place the dock's rectangle lives.
function treeDockCells(regions){
  const t=regions&&regions.tree;
  if(!t||!t.surfaces) return 0;
  for(const s of t.surfaces) if(s.kind==="dock"&&s.x===0&&s.w<(scene.w||0)) return s.w;
  return 0;
}
