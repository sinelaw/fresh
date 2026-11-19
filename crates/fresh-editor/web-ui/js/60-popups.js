// Native popups (completion / hover / action / list / text).
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native popups (completion / hover / action / list / text) ----------
// The editor owns content, selection and scroll; we render natively and forward
// row clicks / wheel back through handle_mouse at the popup's content cells, so
// the existing popup hit-tester resolves them (no re-implemented logic).
// One styled run of a text/markdown popup line, in the same shape a buffer
// cell run arrives in ({t,fg,bg,b,i,u,r}) — the editor highlights hover docs
// with the real grammar registry, and the terminal draws every one of those
// colours, so the browser does too. The ink is written as INLINE style on
// purpose: a web theme skins the popup's frame (background, border, radius),
// never the editor colours inside it, and inline style outranks every skin
// rule without needing !important anywhere.
/** function popupRun(PopupSpan) => El */
function popupRun(run){
  const el=document.createElement("span");
  el.textContent=run.t;
  // REVERSED swaps the pair, exactly like the cell renderer does.
  const fg=run.r?run.bg:run.fg, bg=run.r?run.fg:run.bg;
  if(fg) el.style.color=fg;
  if(bg) el.style.background=bg;
  if(run.b) el.style.fontWeight="700";
  if(run.i) el.style.fontStyle="italic";
  if(run.u) el.style.textDecoration="underline";
  return el;
}

/** function popupEl(Popup) => El */
function popupEl(p){
  // `popup-<kind>` lets a skin tell completion, hover, actions and lists apart.
  const el=div("popup popup-"+p.kind); place(el,p.rect);
  // Report pointer moves on the card (which `onChrome` forwarding skips) so
  // the editor keeps a hover card up while the pointer is on it (#3733).
  // Shares the document handler's de-dupe, so a move back is reported too.
  { const c=rectCell(p.rect);
    el.onmousemove=()=>{
      const lm=lastMoveCell;
      if(lm&&lm.col===c.col&&lm.row===c.row) return;
      lastMoveCell={col:c.col,row:c.row,ctrl:false};
      sendMouse({kind:"moved",col:c.col,row:c.row});
    }; }
  // Lay the popup out on the editor's cells (head = rows above the content,
  // body = content rows); free layout overflowed the box.
  const title=p.title, desc=p.description;
  const cr=p.contentRect, n=Math.max(0,cr.h), start=p.scrollOffset||0;
  // Without title or description, the card is just the content in a thin frame.
  const bare=!title&&!desc, pad=4;
  if(bare){
    el.style.top=`${px(cr.y,CH)-pad}px`;
    el.style.height=`${px(n,CH)+2*pad}px`;
    el.style.paddingTop=el.style.paddingBottom=`${pad}px`;
    el.style.boxSizing="border-box";
  }
  const head=div("popup-head"); head.style.height=bare?"0px":`${px(Math.max(0,cr.y-p.rect.y),CH)}px`;
  if(title){ const t=div("popup-title"); t.textContent=title; head.appendChild(t); }
  if(desc){ const dd=div("popup-desc"); dd.textContent=desc; head.appendChild(dd); }
  el.appendChild(head);
  const content=p.content;
  // A list is a well a skin may dress; hover lines keep the editor's ink.
  const body=div("popup-body"+(content.type==="list"?" ui-well":"")); body.style.height=`${px(n,CH)}px`;
  if(content.type==="list"){
    const items=content.items||[];
    for(let j=0;j<n;j++){
      const idx=start+j, it=items[idx]; if(!it) break;
      const row=div("popup-row"+(idx===content.selected?" sel":"")+(it.disabled?" disabled":""));
      const icon=it.icon, detail=it.detail;
      if(icon){ const ic=document.createElement("span"); ic.className="picon"; ic.textContent=icon; row.appendChild(ic); }
      const tx=document.createElement("span"); tx.className="ptext2"; tx.textContent=it.text; row.appendChild(tx);
      if(detail){ const dt=document.createElement("span"); dt.className="pdetail"; dt.textContent=detail; row.appendChild(dt); }
      const cell={col:cr.x+1,row:cr.y+j};
      row.onmousedown=e=>{ if(it.disabled) return; e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:cell.col,row:cell.row}); };
      body.appendChild(row);
    }
  } else {
    // Editor ink (see popupRun): the body stands on the editor's own popup
    // ground so the runs keep the contrast they were chosen for under every
    // chrome skin.
    body.classList.add("ink");
    const lines=content.lines||[];
    // The window is the editor's, for a text popup as much as for a list: the
    // scene now carries the offset of the viewport the TUI scrolls (see
    // `popups_view`), so slicing against it shows exactly the rows the
    // terminal shows, and the wheel below moves both at once.
    for(let j=0;j<n;j++){
      const ln=lines[start+j]; if(ln===undefined) break;
      const d=div("popup-line");
      for(const run of ln) d.appendChild(popupRun(run));
      body.appendChild(d);
    }
  }
  el.appendChild(body);
  listen(el,"wheel",e=>{ e.stopPropagation(); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:cr.x+1,row:cr.y,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  return el;
}

