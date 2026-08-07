// Native menu bar + dropdowns.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native menu bar ----------------------------------------------------
function menuBarEl(reg){
  const rect=reg.menubar;            // menubar region IS the rect {x,y,w,h}
  const bar=div("region menubar"); place(bar,{x:rect.x,y:rect.y,w:rect.w,h:1});
  const barRow=rect.y;
  reg.menus.forEach((m,i)=>{
    // Visibility is decided once in the core (MenuEntry.visible, from the shared
    // is_menu_visible); skip hidden menus instead of re-deriving here.
    if(m.visible===false) return;
    const el=div("menu"+(reg.menuOpen===i?" open":""));
    el.textContent=m.label;
    // Position each title at its EDITOR cell x (not CSS flow), so the title sits
    // exactly where the editor hit-tests it and the dropdown — positioned at the
    // same cell coords — opens directly beneath it.
    if(m.x!=null){ el.style.position="absolute";
      el.style.left=px(m.x-rect.x,CW)+"px";
      if(m.w) el.style.width=px(m.w,CW)+"px";
      el.style.padding="0"; el.style.justifyContent="center"; }
    // forward to the real editor at the menu label's center cell -> it opens/toggles.
    const col=(m.x??rect.x)+Math.floor((m.w||1)/2);
    el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col,row:barRow}); };
    // Hover switches menus only while one is open (mirrors the TUI). Uses the
    // menu's editor cell, and `mousemove` so a stationary cursor never fights
    // keyboard navigation.
    el.onmousemove=()=>{ if(reg.menuOpen!=null) hoverMove(col,barRow,"menu:"+i); };
    bar.appendChild(el);
  });
  return bar;
}

// Walk submenuPath to the items list shown at a given submenu depth (>=1).
function submenuItems(reg, depth){
  let items=reg.menus[reg.menuOpen]?.items||[];
  for(let d=0; d<depth; d++){
    const it=items[reg.submenuPath[d]];
    if(!it||it.kind!=="submenu") return [];
    items=it.items||[];
  }
  return items;
}

// Vertical compression for the open menu tree. In the TUI a separator is a full
// character row, so the core emits it as a 1-cell row — on the web that renders
// as a hairline floating in a ~21px void, which reads as "menus have way too
// much padding". This shrinks every separator row to a compact macOS separator
// (~11px) and pulls the rows below it up, so groups sit tight the way a native
// AppKit menu does. Item rows keep their full cell height (already ~correct).
//
// Everything — items, separators, backing panels, submenu boxes — is placed
// through this same per-depth remap, so alignment is preserved: a submenu
// re-anchors to its (now shifted) parent row, and each level compresses its own
// separators on top of that anchor. Hit-testing is unaffected: clicks/hover are
// dispatched by each row's STORED cell (rectCell), never by its pixel position,
// so moving the box up doesn't change which item the editor resolves.
function menuCompression(reg){
  const SEP_PX = Math.max(8, Math.round(CH*0.52));   // compact separator slot
  const rowExtra = CH - SEP_PX;                        // px removed per sep row
  const PAD = Math.max(4, Math.round(CH*0.26));        // target panel top/bottom inset (~5px)
  const sepByDepth = {};                              // depth -> Set(cell-y of seps)
  const addSep=(d,y)=>{ (sepByDepth[d]=sepByDepth[d]||new Set()).add(y); };
  const topItems = reg.menus[reg.menuOpen]?.items||[];
  for(const di of (reg.dropdown?.items||[])) if(topItems[di.index]?.kind==="sep") addSep(0, di.rect.y);
  for(const su of (reg.dropdown?.submenus||[])){ const list=submenuItems(reg, su.depth);
    if(list[su.index]?.kind==="sep") addSep(su.depth, su.rect.y); }
  // Sum the sep-shrink accumulated ABOVE cell-y within one depth's own stack.
  const shrinkAbove=(d,y)=>{ let s=0; const set=sepByDepth[d]; if(set) for(const sy of set) if(sy<y) s+=rowExtra; return s; };
  // Per-depth backing box + item extents (cells), to compress the panel's own
  // top/bottom padding row (~1 full cell in the TUI border) down to PAD.
  const box={}; if(reg.dropdown?.rect) box[0]=reg.dropdown.rect;
  for(const b of (reg.dropdown?.submenuBoxes||[])) box[b.depth]=b.rect;
  const itemYs={};
  for(const di of (reg.dropdown?.items||[])) (itemYs[0]=itemYs[0]||[]).push(di.rect.y);
  for(const su of (reg.dropdown?.submenus||[])) (itemYs[su.depth]=itemYs[su.depth]||[]).push(su.rect.y);
  const topCut={}, botCut={};   // px removed from a depth's top / bottom padding
  for(const d of Object.keys(box).map(Number)){
    const bx=box[d], ys=itemYs[d];
    if(!bx || !ys || !ys.length){ topCut[d]=0; botCut[d]=0; continue; }
    topCut[d]=Math.max(0, (Math.min(...ys)-bx.y)*CH - PAD);
    botCut[d]=Math.max(0, ((bx.y+bx.h)-(Math.max(...ys)+1))*CH - PAD);
  }
  // panelShift: how far each depth's PANEL moves up. Depth 0's panel top stays
  // anchored under the menu bar; a submenu's panel tracks its parent item's total
  // upward shift so it stays aligned (the −topCut[d] cancels the extra its own
  // items get, keeping the submenu's first item level with the parent row).
  const boxTop={0: box[0]?.y ?? 0};
  for(const d in box) boxTop[d]=box[d].y;
  const panelShift={0:0};
  for(const d of Object.keys(box).map(Number).sort((a,b)=>a-b)){
    if(d===0) continue;
    panelShift[d]=(panelShift[d-1]||0)+shrinkAbove(d-1, boxTop[d])+(topCut[d-1]||0)-(topCut[d]||0);
  }
  // Items shift by their panel's shift + own seps above + their own top-pad cut.
  const itemShift=(d,y)=>(panelShift[d]||0)+shrinkAbove(d,y)+(topCut[d]||0);
  const sepsInside=(d,rect)=>{ let e=0; const set=sepByDepth[d];
    if(set) for(const sy of set) if(sy>=rect.y && sy<rect.y+rect.h) e+=rowExtra; return e; };
  return {
    SEP_PX,
    isSep:(d,y)=> sepByDepth[d]?.has(y)||false,
    // items / separators / labels
    top:(d,y)=> px(y,CH) - itemShift(d,y),
    height:(d,rect)=> sepByDepth[d]?.has(rect.y) ? SEP_PX : px(rect.h,CH) - sepsInside(d,rect),
    // backing panels — top tracks the parent shift (no per-item pad cut), height
    // hugs the now-compressed content (drop the excess top+bottom padding + seps).
    panelTop:(d,y)=> px(y,CH) - (panelShift[d]||0),
    panelH:(d,rect)=> px(rect.h,CH) - sepsInside(d,rect) - (topCut[d]||0) - (botCut[d]||0),
  };
}

// One native dropdown row, positioned at the pipeline's cell rect.
// `hi` says whether the editor currently highlights this row. `comp`/`depth`
// apply the separator compression above (top/height are remapped, x/width and
// the click cell stay exactly as the editor reported them).
function itemRow(item, rect, hi, comp, depth){
  if(!item) return null;
  if(item.kind==="sep"){ const s=div("msep"); place(s,rect);
    const slot = comp ? comp.SEP_PX : CH;
    const top  = comp ? comp.top(depth,rect.y) : px(rect.y,CH);
    s.style.height="1px"; s.style.top=(top+(slot-1)/2)+"px"; return s; }
  if(item.kind==="label"){ const l=div("mlabel"); place(l,rect);
    if(comp){ l.style.top=comp.top(depth,rect.y)+"px"; l.style.height=comp.height(depth,rect)+"px"; }
    l.style.lineHeight=CH+"px"; l.textContent=item.label; return l; }
  const cell=rectCell(rect);
  const row=div("mitem"+(hi?" hi":"")+(item.enabled===false?" disabled":""));
  place(row,rect);
  if(comp){ row.style.top=comp.top(depth,rect.y)+"px"; row.style.height=comp.height(depth,rect)+"px"; }
  row.style.lineHeight=CH+"px";
  const check = item.checked===true?"✓":"";
  const arrow = item.kind==="submenu"?'<span class="arrow">›</span>':"";
  const accel = item.accel?`<span class="accel">${esc(item.accel)}</span>`:"";
  row.innerHTML=`<span class="lab"><span class="check">${check}</span>${esc(item.label)}</span>${accel}${arrow}`;
  // hover highlights via `mousemove` (never fires on a DOM rebuild under a
  // stationary cursor), so arrow-key navigation isn't reset by the mouse.
  row.onmousemove=()=>hoverMove(cell.col,cell.row,"item:"+cell.col+","+cell.row);
  row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation();
    if(item.enabled===false) return;
    sendMouse({kind:"down",button:"left",col:cell.col,row:cell.row}); };
  return row;
}

// Widen each dropdown panel (and its rows) to fit the WIDEST row's real
// rendered content. The core sizes menus in monospace cells, but web chrome
// draws in a proportional font, so the char-count box can be too narrow — the
// longest label+accelerator would overflow the panel (or, with the ellipsis
// guard, truncate). Measuring the natural width here keeps the accelerators in
// one right-aligned column without hard-coding any width or touching the core.
// Placement (top/left, and the click cell) is unchanged, so hit-testing — which
// forwards the item's logical cell, not a pixel — is unaffected.
function fitMenuWidths(host){
  host.querySelectorAll(".dropdown").forEach(panel=>{
    const pr=panel.getBoundingClientRect();
    const rows=[...host.querySelectorAll(".mitem,.msep,.mlabel")].filter(m=>{
      const r=m.getBoundingClientRect();
      return r.left>=pr.left-2 && r.left<pr.right && r.top>=pr.top-2 && r.bottom<=pr.bottom+2;
    });
    const items=rows.filter(m=>m.classList.contains("mitem"));
    if(!items.length) return;
    // Rows are inset within the panel (the cell box reserves a border column);
    // widen the ROWS to their content but grow the PANEL by that same inset on
    // each side, so a widened row never spills past the panel's edge (which
    // made the selection bar overhang the right border).
    const inset=Math.max(0, Math.round(items[0].getBoundingClientRect().left - pr.left));
    let content=0;
    for(const m of items){
      const w0=m.style.width;
      m.style.width="max-content";
      content=Math.max(content, Math.ceil(m.getBoundingClientRect().width));
      m.style.width=w0;
    }
    if(content+inset*2<=Math.ceil(pr.width)) return;   // the cell box already fits
    panel.style.width=(content+inset*2)+"px";
    for(const m of rows) m.style.width=content+"px";
  });
}

function menuDropdownEls(reg){
  const out=[];
  if(reg.menuOpen==null || !reg.dropdown) return out;
  const comp=menuCompression(reg);
  const xshift={};   // depth -> cells the panel was nudged right (submenu seam fix)
  for(const grp of dropdownPanels(reg, comp, xshift)) out.push(grp);   // solid backing panels
  const path=reg.submenuPath||[];
  // top-level items: highlighted = menuHighlight when no submenu is deeper,
  // otherwise the parent of the open submenu (path[0]).
  const items=reg.menus[reg.menuOpen]?.items||[];
  for(const di of reg.dropdown.items){
    const hi = path.length===0 ? di.index===reg.menuHighlight : di.index===path[0];
    const el=itemRow(items[di.index], di.rect, hi, comp, 0); if(el) out.push(el);
  }
  // expanded submenu levels
  for(const su of (reg.dropdown.submenus||[])){
    const list=submenuItems(reg, su.depth);
    const deepest = su.depth===path.length;
    const hi = deepest ? su.index===reg.menuHighlight : su.index===path[su.depth];
    const el=itemRow(list[su.index], su.rect, hi, comp, su.depth); if(el){
      // Move the item right with its nudged panel (see dropdownPanels) so the
      // 1-cell left inset — and thus the label rhythm and highlight pill — match
      // the top-level menu. The right edge is preserved (width shrinks by the
      // same amount); the click cell (rectCell) is untouched.
      const sh=(xshift[su.depth]||0)*CW;
      if(sh>0){ el.style.left=(px(su.rect.x,CW)+sh)+"px"; el.style.width=Math.max(0,px(su.rect.w,CW)-sh)+"px"; }
      out.push(el);
    }
  }
  return out;
}

// Solid rounded panels behind the top-level dropdown and each submenu level,
// placed on the pipeline's full bordered box rects (`dropdown.rect` /
// `submenuBoxes`) — the same footprint the TUI border occupies, so the panel
// sits flush under the menu bar instead of leaving the border row as a gap.
// Item-union fallback kept for scenes predating the recorded boxes.
function dropdownPanels(reg, comp, shiftsOut){
  const panels=[];
  const union=(rects)=>{
    if(!rects.length) return null;
    const x0=Math.min(...rects.map(r=>r.x)), y0=Math.min(...rects.map(r=>r.y));
    const x1=Math.max(...rects.map(r=>r.x+r.w)), y1=Math.max(...rects.map(r=>r.y+r.h));
    return {x:x0,y:y0,w:x1-x0,h:y1-y0};
  };
  // Collect the backing boxes in depth order: top-level (depth 0), then each
  // expanded submenu level.
  const boxes=[];
  const top=reg.dropdown.rect || union(reg.dropdown.items.map(i=>i.rect));
  if(top) boxes.push({depth:0, rect:{...top}});
  const boxByDepth={};
  for(const b of (reg.dropdown.submenuBoxes||[])) boxByDepth[b.depth]=b.rect;
  const byDepth={};
  for(const su of (reg.dropdown.submenus||[])) (byDepth[su.depth]=byDepth[su.depth]||[]).push(su.rect);
  for(const d of Object.keys(byDepth).map(Number).sort((a,b)=>a-b)){
    const rect=boxByDepth[d] || union(byDepth[d]);
    if(rect) boxes.push({depth:d, rect:{...rect}});
  }
  // The TUI places each submenu one column INTO its parent so their border
  // glyphs share a column (an invisible seam in cells). Web panels are solid
  // rounded, shadowed boxes, so that 1-column overlap reads as two rectangles
  // colliding. Nudge each submenu box's LEFT edge to start flush at its parent's
  // right edge (drop the shared column) so the boxes sit edge-to-edge. Item hit
  // rects (.mitem) are placed separately from these decorative boxes and are
  // left exactly as the editor reports them.
  let prevRight=null;
  for(const b of boxes){
    if(b.depth>=1 && prevRight!=null && b.rect.x<prevRight){
      const shift=prevRight-b.rect.x;
      b.rect.x=prevRight; b.rect.w=Math.max(0, b.rect.w-shift);
      // Record the shift so the item rects (placed separately in
      // menuDropdownEls) can move with the panel — otherwise the submenu items
      // lose the 1-cell inset the panel had, so their labels and the accent
      // highlight would hug (and overhang) the panel's left edge.
      if(shiftsOut) shiftsOut[b.depth]=shift;
    }
    prevRight=b.rect.x + b.rect.w;
    const p=div("dropdown"+(b.depth>=1?" submenu":"")); place(p,b.rect);
    if(comp){ p.style.top=comp.panelTop(b.depth,b.rect.y)+"px"; p.style.height=comp.panelH(b.depth,b.rect)+"px"; }
    panels.push(p);
  }
  return panels;
}

