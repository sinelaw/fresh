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
    // Who gives way when the bar is short is the layout's call, as in the
    // terminal: the left side shrinks, the right never does, and an element
    // the layout already cut (given fewer cells than its text and padding)
    // is the one that yields first, with an ellipsis.
    s.dataset.side=seg.side;
    if(seg.side!=="right" && seg.w<[...seg.text].length+2) s.classList.add("cut");
    const c=rectCell({x:seg.x,y:sb.rect.y,w:seg.w,h:1});
    s.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:c.col,row:c.row}); };
    return s;
  };
  const segs=sb.segments.filter(s=>s.text!=="");
  // Nothing the layout cut, yet the chrome font runs wider than cells: the
  // last element on the left gives way, as the terminal's left side does.
  const left=segs.filter(s=>s.side!=="right");
  const leftEls=left.map(segEl);
  const lastLeft=leftEls[leftEls.length-1];
  if(lastLeft&&!leftEls.some(e=>e.classList.contains("cut"))) lastLeft.classList.add("cut");
  for(const e of leftEls) bar.appendChild(e);
  const spacer=document.createElement("span"); spacer.style.flex="1"; bar.appendChild(spacer);
  for(const seg of segs.filter(s=>s.side==="right")) bar.appendChild(segEl(seg));
  return bar;
}

