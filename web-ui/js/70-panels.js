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
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:c.col,row:c.row}); };
    el.appendChild(row);
  }
  const btns=div("td-buttons");
  const mk=(label,rect,primary)=>{ const b=div("td-btn"+(primary?" primary":"")); b.textContent=label; const c=rectCell(rect);
    b.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:c.col,row:c.row}); }; return b; };
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
  const title=div("fx-title"); title.textContent=fe.title||"Explorer"; el.appendChild(title);
  const list=div("fx-list");
  const n=Math.max(0,(fe.viewportHeight||fe.rect.h)), start=fe.scrollOffset||0;
  for(let j=0;j<n;j++){
    const idx=start+j, r=fe.rows[idx]; if(!r) break;
    const row=div("fx-row"+(idx===fe.selected?" sel":""));
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
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:btn(e),col:cell.col,row:cell.row}); };
    list.appendChild(row);
  }
  el.appendChild(list);
  el.addEventListener("wheel",e=>{ e.stopPropagation(); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:fe.rect.x+1,row:fe.rect.y+1,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  // Right-edge resize handle: the editor treats the explorer's rightmost column
  // as a drag border (handle_file_explorer_border_drag). The .fileexplorer div
  // is in onChrome, so the document drag won't fire here — wire it explicitly.
  el.appendChild(borderDragHandle(fe.rect.x + fe.rect.w - 1, fe.rect.y, fe.rect.h));
  return el;
}

// A 1-cell-wide vertical resize grip at editor column `bx`. Drives a real
// editor drag: mousedown sends a `down` at that cell, pointer moves send
// `drag` at the current column (same row), release sends `up` — exactly what
// the TUI does for the file-explorer / dock borders. Uses window listeners so
// the drag continues even when the pointer leaves the chrome element.
function borderDragHandle(bx, by, h){
  const grip=div("resize-grip");
  grip.style.left=px(bx,CW)+"px"; grip.style.top=px(by,CH)+"px";
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

