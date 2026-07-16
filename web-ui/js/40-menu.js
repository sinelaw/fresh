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

// One native dropdown row, positioned at the pipeline's cell rect.
// `hi` says whether the editor currently highlights this row.
function itemRow(item, rect, hi){
  if(!item) return null;
  if(item.kind==="sep"){ const s=div("msep"); place(s,rect); s.style.height="1px"; s.style.top=px(rect.y+0.5,CH)+"px"; return s; }
  if(item.kind==="label"){ const l=div("mlabel"); place(l,rect); l.style.lineHeight=CH+"px"; l.textContent=item.label; return l; }
  const cell=rectCell(rect);
  const row=div("mitem"+(hi?" hi":"")+(item.enabled===false?" disabled":""));
  place(row,rect); row.style.lineHeight=CH+"px";
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

function menuDropdownEls(reg){
  const out=[];
  if(reg.menuOpen==null || !reg.dropdown) return out;
  for(const grp of dropdownPanels(reg)) out.push(grp);      // solid backing panels
  const path=reg.submenuPath||[];
  // top-level items: highlighted = menuHighlight when no submenu is deeper,
  // otherwise the parent of the open submenu (path[0]).
  const items=reg.menus[reg.menuOpen]?.items||[];
  for(const di of reg.dropdown.items){
    const hi = path.length===0 ? di.index===reg.menuHighlight : di.index===path[0];
    const el=itemRow(items[di.index], di.rect, hi); if(el) out.push(el);
  }
  // expanded submenu levels
  for(const su of (reg.dropdown.submenus||[])){
    const list=submenuItems(reg, su.depth);
    const deepest = su.depth===path.length;
    const hi = deepest ? su.index===reg.menuHighlight : su.index===path[su.depth];
    const el=itemRow(list[su.index], su.rect, hi); if(el) out.push(el);
  }
  return out;
}

// Solid rounded panels behind the top-level dropdown and each submenu level,
// placed on the pipeline's full bordered box rects (`dropdown.rect` /
// `submenuBoxes`) — the same footprint the TUI border occupies, so the panel
// sits flush under the menu bar instead of leaving the border row as a gap.
// Item-union fallback kept for scenes predating the recorded boxes.
function dropdownPanels(reg){
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
    }
    prevRight=b.rect.x + b.rect.w;
    const p=div("dropdown"+(b.depth>=1?" submenu":"")); place(p,b.rect); panels.push(p);
  }
  return panels;
}

