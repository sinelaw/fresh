// Native status bar.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native status bar --------------------------------------------------
/** function statusBarEl(StatusBar) => El */
function statusBarEl(sb){
  const bar=div("region statusbar"); place(bar,sb.rect);
  const segEl=seg=>{
    const s=document.createElement("span"); s.className=(seg.name==="text"?"txt":"seg"); s.dataset.name=seg.name; s.textContent=seg.text;
    if(seg.tone) s.dataset.tone=seg.tone;
    // As in the terminal: the left side shrinks, the right never does, and
    // the element the layout cut yields first.
    s.dataset.side=seg.side;
    if(seg.side!=="right" && seg.cut) s.classList.add("cut");
    const c=rectCell({x:seg.x,y:sb.rect.y,w:seg.w,h:1});
    s.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:c.col,row:c.row}); };
    return s;
  };
  const segs=sb.segments.filter(s=>s.text!=="");
  // Nothing cut but the chrome font runs wide: the last left element yields.
  const left=segs.filter(s=>s.side!=="right");
  const leftEls=left.map(segEl);
  const lastLeft=leftEls[leftEls.length-1];
  if(lastLeft&&!leftEls.some(e=>e.classList.contains("cut"))) lastLeft.classList.add("cut");
  for(const e of leftEls) bar.appendChild(e);
  const spacer=document.createElement("span"); spacer.style.flex="1"; bar.appendChild(spacer);
  for(const seg of segs.filter(s=>s.side==="right")) bar.appendChild(segEl(seg));
  return bar;
}

