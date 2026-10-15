// Native menu bar + dropdowns.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native menu bar ----------------------------------------------------
/** function menuBarEl(Regions) => El */
function menuBarEl(reg){
  const rect=reg.menubar;            // menubar region IS the rect {x,y,w,h}
  if(!rect) throw new Error("menuBarEl: no menubar");   // the caller checks
  const bar=aria(div("region menubar"), "menubar", {}); place(bar,{x:rect.x,y:rect.y,w:rect.w,h:1});
  const barRow=rect.y;
  reg.menus.forEach((m,i)=>{
    // Visibility is decided once in the core (MenuEntry.visible, from the shared
    // is_menu_visible); skip hidden menus instead of re-deriving here.
    if(m.visible===false) return;
    const el=aria(div("menu"+(reg.menuOpen===i?" open":"")), "menuitem",
      {haspopup:"menu", expanded:reg.menuOpen===i});
    el.textContent=m.label;
    // Position each title at its EDITOR cell x (not CSS flow), so the title sits
    // exactly where the editor hit-tests it and the dropdown — positioned at the
    // same cell coords — opens directly beneath it.
    // Flowed (`body.menu-flow`): the titles sit side by side with even
    // padding, as a desktop menu bar's do, instead of at their cell slots —
    // which a proportional chrome font leaves unevenly spaced. Clicks still
    // go to each title's own cell (below); the open dropdown is moved under
    // its title afterwards (`alignMenuDropdown`).
    const mx=m.x, mw=m.w;
    if(mx!=null) el.dataset.cellLeft=String(px(mx-rect.x,CW));
    if(mx!=null&&!document.body.classList.contains("menu-flow")){ el.style.position="absolute";
      el.style.left=`${px(mx-rect.x,CW)}px`;
      if(mw) el.style.width=`${px(mw,CW)}px`;
      el.style.padding="0"; el.style.justifyContent="center"; }
    // forward to the real editor at the menu label's center cell -> it opens/toggles.
    const col=(mx??rect.x)+Math.floor((mw||1)/2);
    el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col,row:barRow}); };
    // Hover switches menus only while one is open (mirrors the TUI). Uses the
    // menu's editor cell, and `mousemove` so a stationary cursor never fights
    // keyboard navigation.
    el.onmousemove=()=>{ if(reg.menuOpen!=null) hoverMove(col,barRow,`menu:${i}`); };
    bar.appendChild(el);
  });
  // Flowed, the titles start where the first one's cell slot does — the bar
  // spans the frame, and what is left of that slot is another surface's (the
  // dock's).
  const first=bar.querySelector(".menu");
  const firstLeft=first?first.dataset.cellLeft:undefined;
  if(firstLeft!=null&&document.body.classList.contains("menu-flow"))
    bar.style.paddingLeft=`${firstLeft}px`;
  return bar;
}

// Walk submenuPath to the items list shown at a given submenu depth (>=1).
/** function submenuItems(Regions, Number) => MenuItem[] */
function submenuItems(reg, depth){
  const mo=reg.menuOpen;
  let items=mo==null?[]:(reg.menus[mo]?.items||[]);
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
/** type MenuComp = {
    SEP_PX: Number,
    isSep: (Number, Number) => Boolean,
    top: (Number, Number) => Number,
    height: (Number, Rect) => Number,
    panelTop: (Number, Number) => Number,
    panelH: (Number, Rect) => Number
} */
/** function menuCompression(Regions) => MenuComp */
function menuCompression(reg){
  const SEP_PX = Math.max(8, Math.round(CH*0.52));   // compact separator slot
  const rowExtra = CH - SEP_PX;                        // px removed per sep row
  const PAD = Math.max(4, Math.round(CH*0.26));        // target panel top/bottom inset (~5px)
  // Per-depth tables are Dicts keyed by String(depth) (what JS makes of a
  // number key anyway).
  /** const sepByDepth: Dict<Number[]> */
  const sepByDepth = {};                              // depth -> distinct cell-y of seps
  const addSep=(d,y)=>{ const s=sepByDepth[String(d)]??[]; if(!s.includes(y)) s.push(y); sepByDepth[String(d)]=s; };
  const dd=reg.dropdown;
  const topItems = submenuItems(reg, 0);
  for(const di of (dd?.items||[])) if(topItems[di.index]?.kind==="sep") addSep(0, di.rect.y);
  for(const su of (dd?.submenus||[])){ const list=submenuItems(reg, su.depth);
    if(list[su.index]?.kind==="sep") addSep(su.depth, su.rect.y); }
  // Sum the sep-shrink accumulated ABOVE cell-y within one depth's own stack.
  const shrinkAbove=(d,y)=>{ let s=0; const set=sepByDepth[String(d)]; if(set) for(const sy of set) if(sy<y) s+=rowExtra; return s; };
  // Per-depth backing box + item extents (cells), to compress the panel's own
  // top/bottom padding row (~1 full cell in the TUI border) down to PAD.
  /** const box: Dict<Rect> */
  const box={}; const dr=dd?.rect; if(dr) box["0"]=dr;
  for(const b of (dd?.submenuBoxes||[])) box[String(b.depth)]=b.rect;
  /** const itemYs: Dict<Number[]> */
  const itemYs={};
  for(const di of (dd?.items||[])){ const ys0=itemYs["0"]??[]; itemYs["0"]=ys0; ys0.push(di.rect.y); }
  for(const su of (dd?.submenus||[])){ const ysd=itemYs[String(su.depth)]??[]; itemYs[String(su.depth)]=ysd; ysd.push(su.rect.y); }
  /** const topCut: Dict<Number> */
  const topCut={};   // px removed from a depth's top / bottom padding
  /** const botCut: Dict<Number> */
  const botCut={};
  for(const k of Object.keys(box)){
    const bx=box[k], ys=itemYs[k];
    if(!bx || !ys || !ys.length){ topCut[k]=0; botCut[k]=0; continue; }
    topCut[k]=Math.max(0, (ys.reduce((a,b)=>Math.min(a,b),Infinity)-bx.y)*CH - PAD);
    botCut[k]=Math.max(0, ((bx.y+bx.h)-(ys.reduce((a,b)=>Math.max(a,b),-Infinity)+1))*CH - PAD);
  }
  // panelShift: how far each depth's PANEL moves up. Depth 0's panel top stays
  // anchored under the menu bar; a submenu's panel tracks its parent item's total
  // upward shift so it stays aligned (the −topCut[d] cancels the extra its own
  // items get, keeping the submenu's first item level with the parent row).
  /** const panelShift: Dict<Number> */
  const panelShift={"0":0};
  for(const k of Object.keys(box).sort((a,b)=>Number(a)-Number(b))){
    const d=Number(k), pb=box[k];
    if(d===0 || !pb) continue;
    const up=String(d-1);
    panelShift[k]=(panelShift[up]||0)+shrinkAbove(d-1, pb.y)+(topCut[up]||0)-(topCut[k]||0);
  }
  // Items shift by their panel's shift + own seps above + their own top-pad cut.
  const itemShift=(d,y)=>(panelShift[String(d)]||0)+shrinkAbove(d,y)+(topCut[String(d)]||0);
  const sepsInside=(d,rect)=>{ let e=0; const set=sepByDepth[String(d)];
    if(set) for(const sy of set) if(sy>=rect.y && sy<rect.y+rect.h) e+=rowExtra; return e; };
  return {
    SEP_PX,
    isSep:(d,y)=> sepByDepth[String(d)]?.includes(y)||false,
    // items / separators / labels
    top:(d,y)=> px(y,CH) - itemShift(d,y),
    height:(d,rect)=> sepByDepth[String(d)]?.includes(rect.y) ? SEP_PX : px(rect.h,CH) - sepsInside(d,rect),
    // backing panels — top tracks the parent shift (no per-item pad cut), height
    // hugs the now-compressed content (drop the excess top+bottom padding + seps).
    panelTop:(d,y)=> px(y,CH) - (panelShift[String(d)]||0),
    panelH:(d,rect)=> px(rect.h,CH) - sepsInside(d,rect) - (topCut[String(d)]||0) - (botCut[String(d)]||0),
  };
}

// One native dropdown row, positioned at the pipeline's cell rect.
// `hi` says whether the editor currently highlights this row. `comp`/`depth`
// apply the separator compression above (top/height are remapped, x/width and
// the click cell stay exactly as the editor reported them).
/** function itemRow(MenuItem, Rect, Boolean, MenuComp, Number) => El | Null */
function itemRow(item, rect, hi, comp, depth){
  if(!item) return null;
  if(item.kind==="sep"){ const s=aria(div("msep"),"separator",{}); place(s,rect);
    const slot = comp ? comp.SEP_PX : CH;
    const top  = comp ? comp.top(depth,rect.y) : px(rect.y,CH);
    s.style.height="1px"; s.style.top=`${top+(slot-1)/2}px`; return s; }
  if(item.kind==="label"){ const l=div("mlabel"); place(l,rect);
    if(comp){ l.style.top=`${comp.top(depth,rect.y)}px`; l.style.height=`${comp.height(depth,rect)}px`; }
    l.style.lineHeight=`${CH}px`; l.textContent=item.label; return l; }
  // Only an action carries enabled/checked/accel; a submenu has none (they
  // read as undefined on it).
  const act = item.kind==="action" ? item : null;
  const cell=rectCell(rect);
  const row=aria(div("mitem"+(hi?" hi":"")+(act?.enabled===false?" disabled":"")),
    act?.checked!=null?"menuitemcheckbox":"menuitem",
    {disabled:act?.enabled===false, checked:act?.checked!=null?act?.checked===true:null,
     haspopup:item.kind==="submenu"?"menu":null, label:item.label});
  place(row,rect);
  if(comp){ row.style.top=`${comp.top(depth,rect.y)}px`; row.style.height=`${comp.height(depth,rect)}px`; }
  row.style.lineHeight=`${CH}px`;
  const check = act?.checked===true?"✓":"";
  const arrow = item.kind==="submenu"?'<span class="arrow">›</span>':"";
  const ak = act?.accel;
  const accel = ak?`<span class="accel">${esc(ak)}</span>`:"";
  row.innerHTML=`<span class="lab"><span class="check">${check}</span>${esc(item.label)}</span>${accel}${arrow}`;
  // hover highlights via `mousemove` (never fires on a DOM rebuild under a
  // stationary cursor), so arrow-key navigation isn't reset by the mouse.
  row.onmousemove=()=>hoverMove(cell.col,cell.row,`item:${cell.col},${cell.row}`);
  row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation();
    if(act?.enabled===false) return;
    sendClick({button:"left",col:cell.col,row:cell.row}); };
  return row;
}

// Widen each dropdown panel (and its rows) to fit the WIDEST row's real
// rendered content. The core sizes menus in monospace cells, but web chrome
// draws in a proportional font, so the char-count box can be too narrow — the
// longest label+accelerator would overflow the panel (or, with the ellipsis
// guard, truncate). Measuring the natural width here keeps the accelerators in
// one right-aligned column without hard-coding any width or touching the core.
// A panel that grows carries every deeper panel that sat flush at its old right
// edge (and their rows) along by the same amount, so the edge-to-edge seam
// `dropdownPanels` established survives; a submenu that flipped LEFT of its
// parent grows leftwards instead, for the same reason. Only pixels move: the
// click cell each row forwards is the editor's logical cell, not a position.
// With a flowed menu bar the open title is not at its cell slot; move the
// dropdown (every level: panels, rows, separators) by the same distance so it
// opens under the title. Hit-testing is by each row's stored cell, so moving
// the boxes changes nothing about what a click selects.
/** function alignMenuDropdown(El) => Undefined */
function alignMenuDropdown(host){
  const title=host.querySelector(".menubar .menu.open");
  const cellLeft=title?title.dataset.cellLeft:undefined;
  if(!title||cellLeft==null) return;
  const dx=title.offsetLeft-parseFloat(cellLeft);
  if(Math.abs(dx)<0.5) return;
  for(const el of host.querySelectorAll(".dropdown,.mitem,.msep,.mlabel"))
    el.style.left=`${parseFloat(el.style.left)+dx}px`;
}

/** function fitMenuWidths(El) => Undefined */
function fitMenuWidths(host){
  const panels=[...host.querySelectorAll(".dropdown")];   // DOM order = depth order
  const rowsOf=(panel)=>{
    const pr=panel.getBoundingClientRect();
    return [...host.querySelectorAll(".mitem,.msep,.mlabel")].filter(m=>{
      const r=m.getBoundingClientRect();
      return r.left>=pr.left-2 && r.left<pr.right && r.top>=pr.top-2 && r.bottom<=pr.bottom+2;
    });
  };
  const shift=(panel, dx)=>{
    const els=[panel, ...rowsOf(panel)];   // membership before anything moves
    for(const el of els) el.style.left=`${parseFloat(el.style.left)+dx}px`;
  };
  panels.forEach((panel, i)=>{
    const before=panel.getBoundingClientRect();
    const rows=rowsOf(panel);
    const items=rows.filter(m=>m.classList.contains("mitem"));
    if(!items.length) return;
    // Rows are inset within the panel (the cell box reserves a border column);
    // widen the ROWS to their content but grow the PANEL by that same inset on
    // each side, so a widened row never spills past the panel's edge (which
    // made the selection bar overhang the right border).
    const inset=Math.max(0, Math.round(items[0].getBoundingClientRect().left - before.left));
    let content=0;
    for(const m of items){
      const w0=m.style.width;
      m.style.width="max-content";
      content=Math.max(content, Math.ceil(m.getBoundingClientRect().width));
      m.style.width=w0;
    }
    if(content+inset*2<=Math.ceil(before.width)) return;   // the cell box already fits
    panel.style.width=`${content+inset*2}px`;
    for(const m of rows) m.style.width=`${content}px`;
    const grow=panel.getBoundingClientRect().right-before.right;
    if(grow<=0) return;
    // Flipped left: this panel's right edge met a shallower panel's left edge.
    const flippedLeft=panels.slice(0,i).some(p=>Math.abs(p.getBoundingClientRect().left-before.right)<0.5);
    if(flippedLeft){ shift(panel,-grow); return; }
    for(const later of panels.slice(i+1))
      if(later.getBoundingClientRect().left>=before.right-0.5) shift(later,grow);
  });
}

/** function menuDropdownEls(Regions) => El[] */
function menuDropdownEls(reg){
  /** const out: El[] */
  const out=[];
  const mo=reg.menuOpen, dd=reg.dropdown;
  if(mo==null || !dd) return out;
  const comp=menuCompression(reg);
  /** const xshift: Dict<Number> */
  const xshift={};   // depth -> cells the panel was nudged right (submenu seam fix)
  for(const grp of dropdownPanels(reg, comp, xshift)) out.push(grp);   // solid backing panels
  const path=reg.submenuPath||[];
  // top-level items: highlighted = menuHighlight when no submenu is deeper,
  // otherwise the parent of the open submenu (path[0]).
  const items=reg.menus[mo]?.items||[];
  for(const di of dd.items){
    const hi = path.length===0 ? di.index===reg.menuHighlight : di.index===path[0];
    const el=itemRow(items[di.index], di.rect, hi, comp, 0); if(el) out.push(el);
  }
  // expanded submenu levels
  for(const su of (dd.submenus||[])){
    const list=submenuItems(reg, su.depth);
    const deepest = su.depth===path.length;
    const hi = deepest ? su.index===reg.menuHighlight : su.index===path[su.depth];
    const sel=itemRow(list[su.index], su.rect, hi, comp, su.depth); if(sel){
      // Move the item right with its nudged panel (see dropdownPanels) so the
      // 1-cell left inset — and thus the label rhythm and highlight pill — match
      // the top-level menu. The right edge is preserved (width shrinks by the
      // same amount); the click cell (rectCell) is untouched.
      const sh=(xshift[String(su.depth)]||0)*CW;
      if(sh>0){ sel.style.left=`${px(su.rect.x,CW)+sh}px`; sel.style.width=`${Math.max(0,px(su.rect.w,CW)-sh)}px`; }
      out.push(sel);
    }
  }
  return out;
}

// Solid rounded panels behind the top-level dropdown and each submenu level,
// placed on the pipeline's full bordered box rects (`dropdown.rect` /
// `submenuBoxes`) — the same footprint the TUI border occupies, so the panel
// sits flush under the menu bar instead of leaving the border row as a gap.
// Item-union fallback kept for scenes predating the recorded boxes.
/** function dropdownPanels(Regions, MenuComp, Dict<Number>) => El[] */
function dropdownPanels(reg, comp, shiftsOut){
  const panels=[];
  /** const union: (Rect[]) => Rect | Null */
  const union=(rects)=>{
    if(!rects.length) return null;
    const min=(ns)=>ns.reduce((a,b)=>Math.min(a,b),Infinity), max=(ns)=>ns.reduce((a,b)=>Math.max(a,b),-Infinity);
    const x0=min(rects.map(r=>r.x)), y0=min(rects.map(r=>r.y));
    const x1=max(rects.map(r=>r.x+r.w)), y1=max(rects.map(r=>r.y+r.h));
    return {x:x0,y:y0,w:x1-x0,h:y1-y0};
  };
  // Collect the backing boxes in depth order: top-level (depth 0), then each
  // expanded submenu level.
  const dd=reg.dropdown;
  if(!dd) throw new Error("dropdownPanels: no dropdown");   // the caller checks
  /** const boxes: {depth: Number, rect: Rect}[] */
  const boxes=[];
  const top=dd.rect ?? union(dd.items.map(i=>i.rect));
  if(top) boxes.push({depth:0, rect:{...top}});
  /** const boxByDepth: Dict<Rect> */
  const boxByDepth={};
  for(const sb of (dd.submenuBoxes||[])) boxByDepth[String(sb.depth)]=sb.rect;
  /** const byDepth: Dict<Rect[]> */
  const byDepth={};
  for(const su of (dd.submenus||[])){ const rs=byDepth[String(su.depth)]??[]; byDepth[String(su.depth)]=rs; rs.push(su.rect); }
  for(const k of Object.keys(byDepth).sort((a,b)=>Number(a)-Number(b))){
    const rect=boxByDepth[k] ?? union(byDepth[k] ?? []);
    if(rect) boxes.push({depth:Number(k), rect:{...rect}});
  }
  // The TUI places each submenu one column INTO its parent so their border
  // glyphs share a column (an invisible seam in cells). Web panels are solid
  // rounded, shadowed boxes, so that 1-column overlap reads as two rectangles
  // colliding. Nudge each submenu box's LEFT edge to start flush at its parent's
  // right edge (drop the shared column) so the boxes sit edge-to-edge. Item hit
  // rects (.mitem) are placed separately from these decorative boxes and are
  // left exactly as the editor reports them.
  /** let prevRight: Number | Null */
  let prevRight=null;
  for(const b of boxes){
    const pr=prevRight;
    if(b.depth>=1 && pr!=null && b.rect.x<pr){
      const shift=pr-b.rect.x;
      b.rect.x=pr; b.rect.w=Math.max(0, b.rect.w-shift);
      // Record the shift so the item rects (placed separately in
      // menuDropdownEls) can move with the panel — otherwise the submenu items
      // lose the 1-cell inset the panel had, so their labels and the accent
      // highlight would hug (and overhang) the panel's left edge.
      if(shiftsOut) shiftsOut[String(b.depth)]=shift;
    }
    prevRight=b.rect.x + b.rect.w;
    const p=aria(div("dropdown"+(b.depth>=1?" submenu":"")), "menu", {}); place(p,b.rect);
    if(comp){ p.style.top=`${comp.panelTop(b.depth,b.rect.y)}px`; p.style.height=`${comp.panelH(b.depth,b.rect)}px`; }
    panels.push(p);
  }
  return panels;
}

