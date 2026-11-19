// Native tab bar.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native tab bar -----------------------------------------------------
//
// **A tab press is held, not clicked** (#3754). The editor's tab node takes
// the pointer on the press and decides on the release: a press that never
// moved is a click (activate), one that moved is a drag — onto another tab
// (reorder, or move to that pane), onto the strip past the tabs (to its end)
// or onto a pane's content (split on an edge, move in the centre). Sending a
// completed click on the press, as this bar used to, left the editor nothing
// to drag. So the press goes out alone, `tabDragMove` forwards the motion and
// `tabDragEnd` the release (both called from the document handlers in
// 80-input.js, which see the pointer wherever it goes).
//
// The bar is laid out by CSS, so a pixel over it is not the editor's cell;
// the pointer is sent as the cell it stands for. Over the pressed tab (the
// same pane's same buffer — not the same column, which a stacked pane's tab
// can share, and which moves when the press scrolls a cut-off tab into view):
// the cell the press went to, so moving within it is no motion at all and the
// editor's own rule alone tells a click from a drag. Over another tab: that
// tab's first or last cell, by which half (the editor's insert-before /
// insert-after rule). Over the bar past the tabs: the cell just after the
// last one. Anywhere else the pane grid is cell-exact: `cellAt`.
/** let tabDrag: {press: Cell, pressKey: String, last: Cell} | Null */
let tabDrag=null;
// A cell coordinate the bar stored in a `data-*` attribute.
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
// The release, at the cell the pointer stands for. When the window loses the
// pointer instead (blur) there is no release to honour: the drag goes back to
// the press cell first, which the editor reads as no drag, so nothing drops.
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
    // Which tab this is, across rebuilds: its pane and its buffer (a group
    // tab, which has none, by its label).
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
        // Right (menu) and middle (close) act on the press: the menu opens at
        // the tab's own first cell, under its left edge, as a terminal opens
        // it under the press.
        if(e.button!==0){ sendClick({button:btn(e),col:t.rect.x,row:t.rect.y}); return; }
        tabDrag={press:c, pressKey:key, last:c};
        sendMouse({kind:"down",button:"left",col:c.col,row:c.row});
      }; }
    // The file-type icon the terminal's tab shows, while `editor.nerd_font_icons` is on.
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
  // Past the last tab: the strip's own end, where a dropped tab goes last.
  const lastTab=p.tabs[p.tabs.length-1];
  const end=lastTab?lastTab.closeRect.x+lastTab.closeRect.w:rect.x;
  bar.dataset.endCol=String(Math.min(end, rect.x+rect.w-1));
  // The `+` (new file / terminal), at the cell the editor laid it out at.
  const newTab=p.newTab;
  if(newTab){
    const plus=div("tab-new"); plus.textContent="+"; plus.title="New tab";
    const pc=rectCell(newTab);
    plus.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:pc.col,row:pc.row}); };
    bar.appendChild(plus);
  }
  return bar;
}
