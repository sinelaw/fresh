// Trust dialog, file explorer, border drag handles.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native workspace-trust dialog --------------------------------------
// Blocking modal. Options / OK / Quit forward to handle_mouse at the pipeline's
// recorded cell rects (the existing handle_workspace_trust_mouse resolves them);
// keyboard already works through handle_key.
function trustDialogEls(t){
  const out=[];
  const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim);
  const el=div("trustdialog"); place(el,t.dialog);
  const title=div("td-title"); title.textContent="⚠  "+t.title; el.appendChild(title);
  if(t.path){ const p=div("td-path"); p.textContent=t.path; el.appendChild(p); }
  if(t.triggers){ const g=div("td-triggers"); g.textContent="Detected: "+t.triggers; el.appendChild(g); }
  for(const o of t.options){
    const row=div("td-opt"+(o.selected?" sel":""));
    const rad=document.createElement("span"); rad.className="td-radio"; rad.textContent=o.selected?"◉":"○"; row.appendChild(rad);
    const tx=document.createElement("div"); tx.className="td-otext";
    tx.innerHTML='<div class="td-olabel">'+esc(o.label)+'</div><div class="td-odesc">'+esc(o.description)+'</div>';
    row.appendChild(tx);
    const c=rectCell(o.rect);
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:c.col,row:c.row}); };
    el.appendChild(row);
  }
  const btns=div("td-buttons");
  const mk=(label,rect,primary)=>{ const b=div("td-btn"+(primary?" primary":"")); b.textContent=label; const c=rectCell(rect);
    b.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:c.col,row:c.row}); }; return b; };
  btns.appendChild(mk(t.okLabel, t.ok, true));
  btns.appendChild(mk(t.quitLabel, t.quit, false));
  el.appendChild(btns);
  out.push(el);
  return out;
}

// ---- native file explorer (sidebar tree) --------------------------------
// The editor owns the tree (expand/collapse/selection/scroll); we render the
// visible window natively and forward row clicks / wheel to handle_mouse at the
// sidebar's content cells, which the existing file-explorer hit-test resolves.
function fileExplorerEl(fe){
  const el=div("region fileexplorer"); place(el,fe.rect);
  // **On the editor's grid.** The editor chose the window for a title of one
  // cell over one cell per row (the same arithmetic the row clicks below
  // assume); a theme's taller rows would push the last of them, often the
  // selection, below the panel's edge. So heights are cells here, and the
  // themes keep everything else.
  const onGrid=e=>{ e.style.height=CH+"px"; e.style.lineHeight=CH+"px"; e.style.boxSizing="border-box";
                    e.style.marginTop=e.style.marginBottom="0"; e.style.paddingTop=e.style.paddingBottom="0"; };
  const title=div("fx-title"); title.textContent=fe.title||"Explorer"; onGrid(title); el.appendChild(title);
  const list=div("fx-list"); list.style.paddingTop=list.style.paddingBottom="0";
  const n=Math.max(0,(fe.viewportHeight||fe.rect.h)), start=fe.scrollOffset||0;
  // Newer cores provide the exact screen-row mapping, including sticky
  // ancestors. Keep the contiguous fallback for compatibility with an older
  // core while the web assets are being refreshed.
  const viewportRows=fe.viewportRows||Array.from({length:n},(_,j)=>start+j);
  for(let j=0;j<n;j++){
    const idx=viewportRows[j], r=fe.rows[idx]; if(!r) break;
    const row=div("fx-row"+(idx===fe.selected?" sel":""));
    onGrid(row);
    row.style.paddingLeft=(6 + r.depth*10)+"px";
    const chev=document.createElement("span"); chev.className="fx-chev";
    chev.textContent=r.isDir?(r.expanded?"▾":"▸"):"";
    row.appendChild(chev);
    const ic=document.createElement("span"); ic.className="fx-icon"+(r.isDir?" dir":"");
    ic.innerHTML=fileIcon(r.name,{dir:r.isDir,expanded:r.expanded}); row.appendChild(ic);
    const name=document.createElement("span"); name.className="fx-name"+(r.isDir?" dir":""); name.textContent=r.name;
    row.appendChild(name);
    const cell={col:fe.rect.x+1,row:fe.rect.y+1+j};   // +1 for the title row
    // Forward the real button so a right-click opens the explorer context menu.
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:btn(e),col:cell.col,row:cell.row}); };
    list.appendChild(row);
  }
  el.appendChild(list);
  el.addEventListener("wheel",e=>{ e.stopPropagation(); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:fe.rect.x+1,row:fe.rect.y+1,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  // Right-edge resize handle: the editor treats the explorer's rightmost column
  // as a drag border (handle_file_explorer_border_drag). The .fileexplorer div
  // is in onChrome, so the document drag won't fire here — wire it explicitly.
  //
  // **Below the header row, above the bottom border** — the rows the TUI gives
  // its own grip (`view/shell/sidebar.rs`, `overlay`: "the top border row is the
  // header's: its close button lives there ... so the grip starts below"). The
  // header row's right end is the close button's three cells, and the
  // explorer's close hides the sidebar, so pressing at `fe.rect.y` closed the
  // panel instead of starting a drag — it vanished the moment you grabbed its
  // edge.
  el.appendChild(borderDragHandle(fe.rect.x + fe.rect.w - 1, fe.rect.y + 1,
                                  Math.max(1, fe.rect.h - 2), fe.rect));
  return el;
}

// A 1-cell-wide vertical resize grip at editor column `bx`, row `by`, `h` rows
// tall. Drives a real editor drag: mousedown sends a `down` at that cell,
// pointer moves send `drag` at the current column (same row), release sends
// `up` — exactly what the TUI does for the file-explorer / dock borders. Uses
// window listeners so the drag continues even when the pointer leaves the
// chrome element.
//
// `bx`/`by` are SCREEN cells, because that is what the editor's hit test reads,
// but CSS offsets are measured from the grip's offset parent. `origin` is that
// parent's own cell position when it has one — the file explorer appends its
// grip inside its own positioned region, so the region's corner is subtracted
// here. The dock's grip goes on the full-grid tree layer, which sits at the
// screen origin, so it passes none and stays absolute. The explorer's grip used
// to be placed at the raw screen cell inside its region, adding that region's
// origin a second time: with a dock open that put the grip out over the editor,
// where other elements covered it and no drag could ever start.
function borderDragHandle(bx, by, h, origin){
  const grip=div("resize-grip");
  const ox=origin?origin.x:0, oy=origin?origin.y:0;
  grip.style.left=px(bx-ox,CW)+"px"; grip.style.top=px(by-oy,CH)+"px";
  grip.style.width=px(1,CW)+"px"; grip.style.height=px(h,CH)+"px";
  grip.onmousedown=e=>{
    e.preventDefault(); e.stopPropagation();
    sendMouse({kind:"down",button:"left",col:bx,row:by});
    const move=ev=>{ const col=Math.max(0,Math.floor((ev.clientX-APPX)/CW));
      sendMouse({kind:"drag",button:"left",col,row:by}); };
    const up=ev=>{ const col=Math.max(0,Math.floor((ev.clientX-APPX)/CW));
      sendMouse({kind:"up",button:"left",col,row:by});
      window.removeEventListener("mousemove",move); window.removeEventListener("mouseup",up); };
    window.addEventListener("mousemove",move); window.addEventListener("mouseup",up);
  };
  return grip;
}
