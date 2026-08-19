// Native popups (completion / hover / action / list / text).
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native popups (completion / hover / action / list / text) ----------
// The editor owns content, selection and scroll; we render natively and forward
// row clicks / wheel back through handle_mouse at the popup's content cells, so
// the existing popup hit-tester resolves them (no re-implemented logic).
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
      row.onmousedown=e=>{ if(it.disabled) return; e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:cell.col,row:cell.row}); };
      body.appendChild(row);
    }
  } else {
    const lines=p.content.lines||[];
    for(let j=0;j<n;j++){ const ln=lines[start+j]; if(ln===undefined) break; const d=div("popup-line"); d.textContent=ln; body.appendChild(d); }
  }
  el.appendChild(body);
  el.addEventListener("wheel",e=>{ e.stopPropagation(); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:cr.x+1,row:cr.y,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  return el;
}

