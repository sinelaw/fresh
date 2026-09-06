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
function treeItemEl(it){
  const el=div("tree-item k-"+it.kind);
  el.dataset.surface=it.surface;
  if(it.key) el.dataset.key=it.key;
  el.style.left=px(it.x,CW)+"px"; el.style.top=px(it.y,CH)+"px";
  el.style.width=px(it.w,CW)+"px"; el.style.height=px(it.h,CH)+"px";
  switch(it.kind){
    case "fill":
      if(it.bg) el.style.background=it.bg;
      break;
    case "border":
      el.style.borderColor=it.fg||"currentColor";
      if(it.bg) el.style.background=it.bg;
      if(it.border==="rounded") el.classList.add("rounded");
      break;
    case "scrim":
      el.classList.add(it.dim?"dim":"opaque");
      if(!it.dim&&it.bg) el.style.background=it.bg;
      break;
    case "lines": {
      if(it.bg) el.style.background=it.bg;
      if(it.fg) el.style.color=it.fg;
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
        const row=div("tree-line"); row.textContent=line;
        row.style.left=dx+"px"; row.style.top=top+"px";
        row.style.font=FONT; row.style.lineHeight=CH+"px"; row.style.height=CH+"px";
        el.appendChild(row);
      });
      break;
    }
    case "scrollbar": {
      if(it.bg) el.style.background=it.bg;
      const th=div("tree-thumb");
      const [top,len]=it.thumb||[0,0];
      th.style.top=px(top,CH)+"px"; th.style.height=px(len,CH)+"px";
      if(it.fg) th.style.background=it.fg;
      el.appendChild(th);
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
    // the fold runs in two bands (retained-mode-ui.md §3.3).
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
  });
  for(const it of (t.items||[])) layer.appendChild(treeItemEl(it));
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
