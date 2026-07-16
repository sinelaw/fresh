// Native text selection, keyboard/mouse/touch input, debug surface, boot.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native browser text selection (hold Alt) ------------------------------
// Every mouse event is normally preventDefault-ed and forwarded to the editor,
// and the stylesheet sets user-select:none — the browser can never build a
// selection of its own, so contexts the editor has no selection model for
// (live terminals, the file explorer, popups, any chrome) had NO way to copy
// text. Holding Alt flips ownership: capture-phase guards stop the forwarding
// handlers (element- and document-level, without preventDefault), body.natsel
// re-enables user-select, and the browser's own drag / double-click selection
// works over ANY text on the page — including the SVG cell grid, i.e. buffer
// interiors and live terminals. Ctrl+C (or right-click → Copy: the context
// menu stays native while Alt is held) copies it; natselText() rebuilds SVG
// selections row-aware so multi-line copies keep their newlines (grid rows
// are separate <text> elements, which Selection.toString() concatenates).
// Alt is free to own: the editor's menu accelerators are Alt+<letter> chords
// (still forwarded), never bare Alt, and its mouse paths only read
// Shift/Ctrl. The gutter stays user-select:none (see the CSS) so cross-pane
// drags copy code, not line numbers.
let natsel=false, natselLeftDown=false;
// Frontend-owned view preference (same class as `zoom`/`paletteCentered`,
// persisted in localStorage, never editor state): whether holding Alt
// engages native selection at all. Ctrl+Alt+S toggles it for users whose
// workflow clashes with Alt-drag.
let natselEnabled = true;
try{ if(localStorage.getItem("fresh.altselect")==="0") natselEnabled=false; }catch(_){}
let natselHintT = null;
function setNatselEnabled(on){
  natselEnabled=on;
  try{ localStorage.setItem("fresh.altselect", on?"1":"0"); }catch(_){}
  if(!on) setNatsel(false);
  // transient feedback in the natsel pill
  const el=document.getElementById("natsel");
  el.querySelector(".rc-label").textContent="Alt text selection: "+(on?"on":"off");
  el.classList.add("show");
  clearTimeout(natselHintT);
  natselHintT=setTimeout(()=>{
    el.classList.toggle("show",natsel);
    el.querySelector(".rc-label").textContent="Select text — drag, then Ctrl+C";
  },1600);
}
function setNatsel(on){
  if(natsel===on) return;
  natsel=on;
  document.body.classList.toggle("natsel",on);
  document.getElementById("natsel").classList.toggle("show",on);
}
document.addEventListener("keydown",e=>{ if(e.key==="Alt"&&!e.repeat&&natselEnabled) setNatsel(true); },true);
document.addEventListener("keyup",e=>{ if(e.key!=="Alt") return;
  e.preventDefault();                    // keep the browser's own Alt menu-focus away
  if(!natselLeftDown) setNatsel(false);  // mid-drag: hold the mode until mouseup
},true);
window.addEventListener("blur",()=>{ natselLeftDown=false; setNatsel(false); });
// Capture-phase guards: while active, stopPropagation keeps every forwarding
// handler blind to the mouse and the missing preventDefault lets the browser
// select. A selection left behind after Alt is released is cleared by the
// next ordinary mousedown, so stale highlights never sit under
// editor-forwarded clicks.
document.addEventListener("mousedown",e=>{
  if(e.altKey&&natselEnabled) setNatsel(true);   // window focused with Alt already held
  if(!natsel){
    const g=window.getSelection&&window.getSelection();
    if(g&&!g.isCollapsed) g.removeAllRanges();
    return;
  }
  if(e.button===0) natselLeftDown=true;
  e.stopPropagation();
},true);
document.addEventListener("mousemove",e=>{ if(natsel) e.stopPropagation(); },true);
document.addEventListener("mouseup",e=>{ if(!natsel) return; e.stopPropagation();
  if(e.button===0){ natselLeftDown=false; if(!e.altKey) setNatsel(false); }
},true);
document.addEventListener("click",e=>{ if(natsel) e.stopPropagation(); },true);
document.addEventListener("dblclick",e=>{ if(natsel) e.stopPropagation(); },true);
// Text of the current native selection. Selections inside an SVG cell grid
// are rebuilt per row — each grid row is its own <text> element, so plain
// Selection.toString() would run rows together — with the grid's cell padding
// trimmed from row ends. Non-SVG (chrome) selections pass through as-is.
function natselText(){
  const sel=window.getSelection&&window.getSelection();
  if(!sel||sel.isCollapsed||!sel.rangeCount) return "";
  const rg=sel.getRangeAt(0);
  const anc=rg.commonAncestorContainer;
  const el=anc.nodeType===1?anc:anc.parentElement;
  const svg=el&&el.closest&&el.closest("svg.cells");
  if(!svg) return sel.toString();
  const partOf=tn=>{
    if(!rg.intersectsNode(tn)) return "";
    const a=tn===rg.startContainer?rg.startOffset:0;
    const b=tn===rg.endContainer?rg.endOffset:tn.length;
    return tn.data.slice(a,b);
  };
  const lines=[];
  for(const rowEl of svg.querySelectorAll("text")){
    if(!rg.intersectsNode(rowEl)) continue;
    let s="";
    for(const tn of rowEl.querySelectorAll("tspan")) for(const n of tn.childNodes) if(n.nodeType===3) s+=partOf(n);
    lines.push(s.replace(/\s+$/,""));
  }
  return lines.join("\n");
}
// Copy event (browser Edit menu, right-click → Copy while Alt is held):
// same row-aware extraction, delivered through the event's clipboardData.
document.addEventListener("copy",e=>{
  const t=natselText();
  if(t&&e.clipboardData){ e.preventDefault(); e.clipboardData.setData("text/plain",t); }
});

// Mouse + wheel: forward to the REAL Editor::handle_mouse at cell coordinates.
const cellAt = e => ({ col: Math.max(0, Math.floor((e.clientX-APPX)/CW)), row: Math.max(0, Math.floor((e.clientY-APPY)/CH)) });
const btn = e => (e.button===2?"right":e.button===1?"middle":"left");
function sendMouse(o){ wsSend({type:"mouse",...o}); }
function sendWidget(o){ wsSend({type:"widget",...o}); }
function sendSettings(o){ wsSend({type:"settings",...o}); }
function sendKbedit(o){ wsSend({type:"kbedit",...o}); }
// click handler that forwards a SettingsHit (kind + indices) to the editor
// `double` carries the browser's click count so entry rows (map/list) can
// open their edit dialog on double-click, exactly like the TUI's
// double-click path (single click only focuses the row).
function setHit(kind,a,b){ return e=>{ e.preventDefault(); e.stopPropagation(); sendSettings({kind,a,b,double:e.detail>=2}); }; }
// Forward a hover as a `moved` event at a chrome element's *editor cell* (not the
// pixel under the pointer — native chrome is laid out by CSS, so pixels don't map
// to the editor's cell columns). Called from `onmousemove` (which, unlike
// `mouseenter`, never fires on a DOM rebuild under a stationary cursor), and
// de-duped so keyboard navigation isn't fought and we don't spam the editor.
let lastHoverKey=null;
function hoverMove(col,row,key){ if(key===lastHoverKey) return; lastHoverKey=key; sendMouse({kind:"moved",col,row}); }
let dragging=false, lastCell=null;
// Chrome (menu bar, dropdown, tabs, status bar) owns its own mouse events and
// forwards them to the editor at the exact cell rects the pipeline reported.
// The document-level handlers are the fallback for buffer interiors, scrollbars
// and separators, so they ignore anything that lands on a chrome element.
const onChrome = e => e.target.closest("#mobile,.menubar,.dropdown,.tabbar,.statusbar,.palette,.popup,.fileexplorer,.trustdialog,.modal-scrim,.widget-surface,.ctxmenu,.auxmodal,.kbedit,.settings-modal");
// `count` carries the browser's click count (event.detail); the bridge primes
// the editor's own double/triple-click detection with it (see apply_mouse).
document.addEventListener("mousedown",e=>{ if(onChrome(e)) return; if(mSheetOpen){ mSheetOpen=false; render(); } const c=cellAt(e); dragging=true; lastCell=c; sendMouse({kind:"down",button:btn(e),col:c.col,row:c.row,count:e.detail,ctrl:e.ctrlKey,shift:e.shiftKey,alt:e.altKey}); });
// Non-drag motion over the buffer is forwarded as `moved` events (TUI parity:
// the terminal receives motion, which dismisses the wave animation/screensaver
// and drives hover). De-duped to at most one send per cell crossing via its
// own lastMoveCell; chrome keeps its element-scoped hoverMove path instead
// (onChrome bails here). The server pushes only on scene change, so idle
// motion over an unchanged buffer produces no frames.
let lastMoveCell=null;
document.addEventListener("mousemove",e=>{
  const c=cellAt(e);
  if(dragging){
    if(lastCell&&c.col===lastCell.col&&c.row===lastCell.row) return;
    lastCell=c;
    sendMouse({kind:"drag",button:"left",col:c.col,row:c.row,ctrl:e.ctrlKey,shift:e.shiftKey,alt:e.altKey});
    return;
  }
  if(onChrome(e)) return;
  if(lastMoveCell&&c.col===lastMoveCell.col&&c.row===lastMoveCell.row) return;
  lastMoveCell=c;
  sendMouse({kind:"moved",col:c.col,row:c.row});
});
document.addEventListener("mouseup",e=>{ dragging=false; if(onChrome(e)) return; const c=cellAt(e); sendMouse({kind:"up",button:btn(e),col:c.col,row:c.row}); });
document.addEventListener("contextmenu",e=>e.preventDefault());
// Desktop: any click that isn't on chrome returns focus to the hidden text
// sink so the next IME composition has somewhere to land (chrome divs aren't
// focusable, but a bare mousedown moves focus to <body>).
document.addEventListener("click",e=>{ if(!onChrome(e)) focusSink(); });
// Settings panes and plugin dock/widget panels emit their full content into the
// DOM, so they scroll natively — forwarding their wheel to the editor would only
// trigger a re-render that resets scrollTop. Let the browser handle those; route
// everything else to the editor at the pointer cell.
const onScrollable = e => e.target.closest(".set-items,.set-cats,.w-list,.widget-surface");
// Ctrl+wheel = app zoom (frontend-owned, like Ctrl+=/-/0 above). Must be a
// non-passive listener so the browser's own page zoom can be preventDefault-ed.
document.addEventListener("wheel",e=>{ if(!e.ctrlKey) return; e.preventDefault();
  setZoom(zoom*(e.deltaY<0?1.1:1/1.1)); },{passive:false});
document.addEventListener("wheel",e=>{ if(e.ctrlKey||onScrollable(e)) return; const c=cellAt(e); const n=Math.min(8,Math.max(1,Math.round(Math.abs(e.deltaY)/40))); sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:c.col,row:c.row,n}); },{passive:true});

// ---- touch pan/scroll (docs §4, mobile Phase A) ---------------------------
// A one-finger pan over the buffer translates into the editor's existing
// wheel path — scrollup/scrolldown (vertical) or scrollleft/scrollright
// (horizontal, which the bridge already maps) at the touch-start cell, one
// step per whole cell panned: plain 1:1 pan, no momentum. A TAP (movement
// under the slop, whatever the duration) is left entirely alone so the
// browser's synthetic mousedown/mouseup still drives the existing click path
// (tap-to-position-cursor, click count included). preventDefault fires only
// while actually panning, so natively-scrolled chrome panels (onChrome) keep
// their own touch scrolling and the page never rubber-bands under a pan.
let tpan=null;
document.addEventListener("touchstart",e=>{
  if(onChrome(e) || e.touches.length!==1){ tpan=null; return; }
  const t=e.touches[0];
  tpan={ id:t.identifier, x0:t.clientX, y0:t.clientY, lx:t.clientX, ly:t.clientY,
         cell:cellAt({clientX:t.clientX,clientY:t.clientY}), axis:null, acc:0 };
},{passive:true});
document.addEventListener("touchmove",e=>{
  if(!tpan) return;
  const t=[...e.changedTouches].find(t=>t.identifier===tpan.id); if(!t) return;
  if(!tpan.axis){                       // pan engages past an 8px slop…
    const dx=t.clientX-tpan.x0, dy=t.clientY-tpan.y0;
    if(Math.hypot(dx,dy)<8) return;
    tpan.axis=Math.abs(dx)>Math.abs(dy)?"x":"y";   // …locked to the dominant axis
  }
  e.preventDefault();                   // we own this gesture: no page rubber-band
  const d = tpan.axis==="y" ? t.clientY-tpan.ly : t.clientX-tpan.lx;
  tpan.lx=t.clientX; tpan.ly=t.clientY; tpan.acc+=d;
  const unit = tpan.axis==="y" ? CH : CW;
  const cells=Math.trunc(tpan.acc/unit);
  if(!cells) return;
  tpan.acc-=cells*unit;
  // finger up / left (negative delta) pulls later content into view
  const kind = tpan.axis==="y" ? (cells<0?"scrolldown":"scrollup")
                               : (cells<0?"scrollright":"scrollleft");
  sendMouse({kind, col:tpan.cell.col, row:tpan.cell.row, n:Math.min(8,Math.abs(cells))});
},{passive:false});
document.addEventListener("touchend",()=>{ tpan=null; },{passive:true});
document.addEventListener("touchcancel",()=>{ tpan=null; },{passive:true});

let rt; window.addEventListener("resize",()=>{ layoutShell(); clearTimeout(rt); rt=setTimeout(resize,150); });
// Re-fit the grid when crossing the mobile/desktop breakpoint (e.g. rotation).
mqMobile.addEventListener("change", resize);

// Test/debug surface. `refresh` is a full-scene resync over HTTP /state (the
// WS hello/frames normally carry everything); `frames`/`seq`/`wsOpen`/
// `lastFrameKeys` expose the push transport's state; `renderedRegions` names
// the region containers the last render pass actually rebuilt (per-region
// patching); `metrics` exposes the measured grid unit + zoom factor plus the
// live #app page origin (ax/ay) so drivers can map grid→page pixels under the
// COSMOS shell inset.
window.fresh = { get scene(){return scene;}, refresh, sendKey, resize, setZoom,
  setPaletteCentered, setNatselEnabled,
  get frames(){return frameCount;}, get seq(){return lastSeq;},
  get wsOpen(){return wsIsOpen();}, get lastFrameKeys(){return lastFrameKeys;},
  get renderedRegions(){return renderedRegions;},
  get paletteCentered(){return paletteCentered;},
  get natselEnabled(){return natselEnabled;},
  get metrics(){ syncAppOrigin(); return {cw:CW, ch:CH, font:FONT, zoom, ax:APPX, ay:APPY}; } };
// Set the mobile class BEFORE the first resize()/layoutShell(): both read
// #app's box, which the class switches between inset (COSMOS shell) and
// full-bleed.
document.body.classList.toggle("mobile", isMobile());
syncAppOrigin();
focusSink();  // desktop: arm the hidden text sink (IME/dead-key path)
connect();    // WS: hello replaces the scene, then resize() fits the grid
