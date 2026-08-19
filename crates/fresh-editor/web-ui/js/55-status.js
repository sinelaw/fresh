// Native status bar.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native status bar --------------------------------------------------
function statusBarEl(sb){
  const bar=div("region statusbar"); place(bar,sb.rect);
  const segEl=seg=>{
    const s=document.createElement("span"); s.className=(seg.name==="text"?"txt":"seg"); s.dataset.name=seg.name; s.textContent=seg.text;
    const c=rectCell({x:seg.x,y:sb.rect.y,w:seg.w,h:1});
    s.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:c.col,row:c.row}); };
    return s;
  };
  const segs=sb.segments.filter(s=>s.text);
  for(const seg of segs.filter(s=>s.side!=="right")) bar.appendChild(segEl(seg));
  const spacer=document.createElement("span"); spacer.style.flex="1"; bar.appendChild(spacer);
  for(const seg of segs.filter(s=>s.side==="right")) bar.appendChild(segEl(seg));
  return bar;
}

