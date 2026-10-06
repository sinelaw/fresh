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

function popupEl(p){
  const el=div("popup"); place(el,p.rect);
  if(p.title){ const t=div("popup-title"); t.textContent=p.title; el.appendChild(t); }
  if(p.description){ const d=div("popup-desc"); d.textContent=p.description; el.appendChild(d); }
  const body=div("popup-body");
  const cr=p.contentRect, n=Math.max(0,cr.h), start=p.scrollOffset||0;
  if(p.content.type==="list"){
    const items=p.content.items||[];
    for(let j=0;j<n;j++){
      const idx=start+j, it=items[idx]; if(!it) break;
      const row=div("popup-row"+(idx===p.content.selected?" sel":"")+(it.disabled?" disabled":""));
      if(it.icon){ const ic=document.createElement("span"); ic.className="picon"; ic.textContent=it.icon; row.appendChild(ic); }
      const tx=document.createElement("span"); tx.className="ptext2"; tx.textContent=it.text; row.appendChild(tx);
      if(it.detail){ const dt=document.createElement("span"); dt.className="pdetail"; dt.textContent=it.detail; row.appendChild(dt); }
      const cell={col:cr.x+1,row:cr.y+j};
      row.onmousedown=e=>{ if(it.disabled) return; e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:cell.col,row:cell.row}); };
      body.appendChild(row);
    }
  } else {
    // Editor ink (see popupRun): the body stands on the editor's own popup
    // ground so the runs keep the contrast they were chosen for under every
    // chrome skin.
    body.classList.add("ink");
    const lines=p.content.lines||[];
    // **The whole content, not the window.** A list's window is the editor's
    // and moves with its selection, but a text / markdown popup (an LSP hover)
    // is drawn through a `viewport().scrollbar()` whose scrolling never reaches
    // the popup's `scroll_offset` — so that offset stays 0 for the life of the
    // popup. Slicing to `contentRect.h` rows against it pinned the view to the
    // first screenful: whenever the editor had to clip the box to fit the
    // window, the rest of the documentation was simply absent, and no wheel or
    // key could bring it back. `.popup-body` is `overflow:auto`, so handing it
    // every line gives the browser a real scrollbar over the same box.
    for(const ln of lines){
      const d=div("popup-line");
      for(const run of ln) d.appendChild(popupRun(run));
      body.appendChild(d);
    }
    // Follow an offset the editor *did* move, so the two never disagree.
    if(start) requestAnimationFrame(()=>{ body.scrollTop=start*CH; });
  }
  el.appendChild(body);
  // The wheel over a LIST belongs to the editor, which owns that window and
  // its selection. A text body scrolls itself now, so its wheel stays in the
  // browser — forwarding it as well would fight the native scroll.
  if(p.content.type==="list")
    el.addEventListener("wheel",e=>{ e.stopPropagation(); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:cr.x+1,row:cr.y,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  return el;
}

