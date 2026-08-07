// Native tab bar.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native tab bar -----------------------------------------------------
function tabBarEl(p){
  const bar=div("region tabbar"); place(bar,p.tabBar);
  for(const t of p.tabs){
    const el=div("tab"+(t.active?" active":"")+(t.modified?" modified":""));
    if(t.rect){ const c=rectCell(t.rect);
      el.onmousedown=e=>{ if(e.target.classList.contains("x")) return; e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:btn(e),col:c.col,row:c.row}); }; }
    const name=document.createElement("span"); name.className="name"; name.textContent=t.label; el.appendChild(name);
    const dot=document.createElement("span"); dot.className="dot"; el.appendChild(dot);
    const x=document.createElement("span"); x.className="x"; x.textContent="×";
    if(t.closeRect){ const cc=rectCell(t.closeRect);
      x.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:cc.col,row:cc.row}); }; }
    el.appendChild(x);
    bar.appendChild(el);
  }
  return bar;
}

