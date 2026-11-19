// Native tab bar.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native tab bar -----------------------------------------------------
//
// A tab press is held, not clicked (#3754): the editor tells click from drag
// on release, so down/drag/up are sent separately (tabDragMove/tabDragEnd are
// called from 80-input.js). The CSS-laid bar maps the pointer to the cell it
// stands for: the press cell over the pressed tab, the nearer edge cell of
// another tab, the end cell past the tabs.
/** let tabDrag: {press: Cell, pressKey: String, last: Cell} | Null */
let tabDrag=null;
/** function dnum(String | Undefined) => Number */
function dnum(s){ return parseInt(s??"0",10); }
/** function tabDragCell({press: Cell, pressKey: String, last: Cell}, UiEvent) => Cell */
function tabDragCell(d, e){
  const tab=e.target.closest(".tabbar .tab");
  if(tab){
    if(tab.dataset.key===d.pressKey) return d.press;
    const r=tab.getBoundingClientRect();
    const left=e.clientX<r.left+r.width/2;
    return {col:dnum(left?tab.dataset.l:tab.dataset.r), row:dnum(tab.dataset.row)};
  }
  const bar=e.target.closest(".tabbar");
  if(bar) return {col:dnum(bar.dataset.endCol), row:dnum(bar.dataset.row)};
  return cellAt(e);
}
/** function tabDragMove(UiEvent) => Undefined */
function tabDragMove(e){
  const d=tabDrag; if(!d) return;
  const c=tabDragCell(d, e);
  if(c.col===d.last.col&&c.row===d.last.row) return;
  d.last=c;
  document.body.classList.toggle("tabdragging", c!==d.press);
  sendMouse({kind:"drag",button:"left",col:c.col,row:c.row});
}
// On blur (no release), return to the press cell first so nothing drops.
/** function tabDragEnd(UiEvent | Null) => Undefined */
function tabDragEnd(e){
  const d=tabDrag; if(!d) return;
  tabDrag=null;
  document.body.classList.remove("tabdragging");
  if(!e) sendMouse({kind:"drag",button:"left",col:d.press.col,row:d.press.row});
  const c=e?tabDragCell(d, e):d.press;
  sendMouse({kind:"up",button:"left",col:c.col,row:c.row});
}
window.addEventListener("blur",()=>tabDragEnd(null));

/** function tabBarEl(Pane, Rect) => El */
function tabBarEl(p, rect){
  const bar=div("region tabbar"); place(bar,rect);
  bar.dataset.row=String(rect.y);
  for(const t of p.tabs){
    const el=div("tab"+(t.active?" active":"")+(t.modified?" modified":"")
      +(t.preview?" preview":"")+(t.binary?" binary":"")+(t.group?" group":""));
    // Stable across rebuilds: pane + buffer (a group tab by its label).
    const bid=t.bufferId;
    const key=`${p.leaf}:`+(bid===null?"g:"+t.label:String(bid));
    el.dataset.key=key;
    el.dataset.l=String(t.rect.x);
    el.dataset.r=String(t.rect.x+Math.max(1,t.rect.w)-1);
    el.dataset.row=String(t.rect.y);
    { const c=rectCell(t.rect);
      el.onmousedown=e=>{
        if(e.target.classList.contains("x")) return;
        e.preventDefault(); e.stopPropagation();
        // Right (menu) and middle (close) act on the press, at the tab's first cell.
        if(e.button!==0){ sendClick({button:btn(e),col:t.rect.x,row:t.rect.y}); return; }
        tabDrag={press:c, pressKey:key, last:c};
        sendMouse({kind:"down",button:"left",col:c.col,row:c.row});
      }; }
    const icon=t.icon;
    if(icon){
      const ic=document.createElement("span"); ic.className="ticon"; ic.textContent=icon.glyph;
      const color=icon.color; if(color) ic.style.setProperty("--icon-color",color);
      el.appendChild(ic);
    }
    const name=document.createElement("span"); name.className="name"; name.textContent=t.label; el.appendChild(name);
    const dot=document.createElement("span"); dot.className="dot"; el.appendChild(dot);
    const x=document.createElement("span"); x.className="x"; x.textContent="×";
    { const cc=rectCell(t.closeRect);
      x.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:cc.col,row:cc.row}); }; }
    el.appendChild(x);
    bar.appendChild(el);
  }
  // Past the last tab: where a dropped tab goes last.
  const lastTab=p.tabs[p.tabs.length-1];
  const end=lastTab?lastTab.closeRect.x+lastTab.closeRect.w:rect.x;
  bar.dataset.endCol=String(Math.min(end, rect.x+rect.w-1));
  const newTab=p.newTab;
  if(newTab){
    const plus=div("tab-new ui-chip"); plus.textContent="+"; plus.title="New tab";
    const pc=rectCell(newTab);
    plus.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:pc.col,row:pc.row}); };
    bar.appendChild(plus);
  }
  return bar;
}
