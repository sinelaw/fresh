// Natively laid-out surfaces (`regions.native`, crates/fresh-editor/src/view/native.rs).
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
//
// The display list (`regions.tree`, 72-tree.js) is what the shell's layout
// drew, item by item at character cells — faithful, and so it reads as a
// terminal. `regions.native` carries the same surfaces as content, and this
// file lays them out as a desktop app would: real buttons, the chrome font,
// pixel spacing. While a surface is drawn here its display-list items are
// hidden (45-tree.css), never removed: the tree still lays them out, because
// every press goes back to it.
//
// **Input is by key, never by cell.** Each control carries the key of the tree
// node it mirrors, and `{type:"node", key}` presses that node where the
// shell's layout put it (`apply_node` in webui/mod.rs). The node's own handler
// decides what the press means — the same code a terminal click runs.

// Which web themes lay surfaces out natively. The others keep the display
// list's faithful cell rendering: Cosmos and Winamp are designed around it.
// `fresh.nativePanels` in localStorage ("on"/"off") overrides either way.
const NATIVE_PANEL_THEMES = ["compact", "macos", "macos-dark"];
/** function nativePanelsOn() => Boolean */
function nativePanelsOn(){
  /** let pref: String | Null */
  let pref=null;
  try { pref=localStorage.getItem("fresh.nativePanels"); } catch(_) {}
  if(pref==="on") return true;
  if(pref==="off") return false;
  return NATIVE_PANEL_THEMES.includes(webTheme);
}

// Whether a cell lies under a natively laid-out panel. Such a panel puts its
// fields elsewhere than the cells the terminal's cursor marks, and its own
// focused input carries the caret — so no cell caret is drawn there.
/** function underNative(NativeView, Number, Number) => Boolean */
function underNative(n, x, y){
  if(!document.body.classList.contains("native-panels")) return false;
  const inRect=r=>x>=r.x && x<r.x+r.w && y>=r.y && y<r.y+r.h;
  return n.panels.some(p=>{ const f=p.frame; return inRect(p.rect)||(f!==undefined&&inRect(f)); });
}

/** function sendNode(TreeKey, String | Undefined) => Undefined */
function sendNode(key, action){ wsSend({type:"node", key, action:action||"press"}); }

// Accelerators are underlined only while Alt is held, as native dialogs do;
// the letters still answer without it (the editor's confirm keys).
for(const t of ["keydown","keyup"]) listen(document, t, e=>{
  if(e.key==="Alt") document.body.classList.toggle("alt-held", t==="keydown");
}, {capture:true});
window.addEventListener("blur", ()=>document.body.classList.remove("alt-held"));

// A control that never takes the keyboard: the editor owns every key, and a
// focused <button> would answer Enter/Space itself as well. Pressing happens
// on release over the control, as with any native button.
/** function nativeButton(String, TreeKey) => El */
function nativeButton(cls, key){
  const b=document.createElement("button");
  b.type="button"; b.className=cls; b.tabIndex=-1;
  b.addEventListener("mousedown", e=>{ e.preventDefault(); e.stopPropagation(); });
  b.addEventListener("click", e=>{ e.preventDefault(); e.stopPropagation(); sendNode(key, undefined); });
  return b;
}

// ---- the confirmation card ----------------------------------------------
/** function confirmCardEls(ConfirmCard) => El[] */
function confirmCardEls(c){
  const scrim=div("native-scrim");
  // The card is modal: a press outside it answers nothing (the terminal's
  // layer swallows it the same way).
  scrim.addEventListener("mousedown", e=>{ e.preventDefault(); e.stopPropagation(); });
  const card=div("native-card native-confirm");
  aria(card, "alertdialog", {modal:true, label:c.title, description:null});
  if(c.body) card.setAttribute("aria-describedby","nc-body");
  card.addEventListener("mousedown", e=>{ e.stopPropagation(); });
  const title=div("nc-title"); title.textContent=c.title; card.appendChild(title);
  if(c.body){ const p=div("nc-body"); p.id="nc-body"; p.textContent=c.body; card.appendChild(p); }
  if(c.detail){ const d=div("nc-detail"); d.textContent=c.detail; card.appendChild(d); }
  const row=div("nc-buttons");
  for(const b of c.buttons){
    const el=nativeButton("nc-btn"+(b.selected?" selected":"")+(b.destructive?" destructive":"")
      +(b.escape?" escape":""), b.key);
    const [pre, acc, post]=b.label;
    el.append(pre);
    if(acc){ const u=document.createElement("span"); u.className="accel"; u.textContent=acc; el.appendChild(u); }
    if(post) el.append(post);
    aria(el, "button", {label:(pre+(acc||"")+(post||"")).trim(), keyshortcuts:acc?acc.toUpperCase():null});
    row.appendChild(el);
  }
  card.appendChild(row);
  scrim.appendChild(card);
  return [scrim];
}

// ---- plugin panels --------------------------------------------------------
// The panel's frame (title bar, close box, border) stays the display list's;
// its interior is laid out here from the spec, in the interior's own
// rectangle, by the widget renderer the toolbar uses (`widgetEl`,
// 65-widgets.js). Presses go back as the control's identity (`routeWidget`).
/** function nativePanelEl(NativePanel) => El */
function nativePanelEl(p){
  // A floating panel is a card over its whole frame — its title and close box
  // drawn here too — with the interior laid out in what is left of it.
  const frame=p.kind==="floating"?p.frame:undefined, framed=frame!==undefined;
  const el=div("native-panel np-"+p.kind+(p.keyboard?" kb":"")+(framed?" card":"")+(p.anchored?" anchored":""));
  place(el, frame??p.rect);
  // An anchored pop-up hugs its content, as the terminal's does — natively
  // laid out, that content is not the cells the terminal measured, so the
  // card takes its height (and any extra width) from what it holds.
  // A centred card hugs its content too, still centred on its frame.
  if(framed&&!p.anchored) hugHeight(el);
  if(framed&&p.anchored){
    el.style.height="auto";
    el.style.minWidth=el.style.width; el.style.width="max-content";
    el.style.maxHeight="calc(100% - "+el.style.top+" - 4px)";
  }
  el.dataset.slot=p.kind;
  // A page scrolls where the editor scrolls it, not by itself.
  if(p.page) el.classList.add("paged");
  // A strip — a toolbar or key-hint row a buffer group mounts in a pane a
  // cell or two tall — is laid out on that height, not padded past it.
  if(p.kind==="pane"&&p.rect&&p.rect.h<=3){ el.classList.add("strip"); el.style.setProperty("--cell-h", CH+"px"); }
  // A floating panel is a dialog (modal while centred); a docked one a region.
  if(p.kind==="floating") aria(el, "dialog", {modal:!p.anchored, label:p.title||null});
  else aria(el, "region", {label:p.title||p.kind});
  let body=el;
  if(framed){
    const closeKey=p.closeKey;
    if(p.title||closeKey){
      const head=div("np-head");
      const t=div("np-title"); t.textContent=p.title||""; head.appendChild(t);
      if(closeKey){
        const x=nativeButton("np-close", closeKey);
        x.setAttribute("aria-label","Close"); x.textContent="×";
        head.appendChild(x);
      }
      el.appendChild(head);
    }
    body=div("np-body"); el.appendChild(body);
  }
  // A press on the interior's own ground (between controls) is the panel's:
  // the cell mapping still forwards it, so it lands where the terminal's
  // would — focusing the panel — without reaching through to what is under.
  /** const ctx: WidgetCtx */
  const ctx={kind:"panel", slot:p.kind, section:p.section, pane:p.pane, focusKey:p.focusKey,
    instances:p.instances, native:true};
  body.appendChild(widgetEl(p.spec, ctx));
  // Keep the selected row of a list or tree in view as the keyboard moves it:
  // the page scrolls its own lists.
  requestAnimationFrame(()=>{
    alignFieldLabels(el);
    if(p.page) syncPage(el, p.page);
    for(const s of el.querySelectorAll(".w-list-row.sel,.w-list-card.sel,.w-tree-row.sel")){
      s.scrollIntoView({block:"nearest"});
    }
  });
  return el;
}

// A page is scrolled by the editor — the wheel, its keys, a card's "jump ↓"
// all move the page's window there — and laid out here in other heights, so
// rows are placed by the widgets on them: each mark is the content row a
// widget starts on, and the view's top is put where the terminal window's
// is, exactly at a widget and in proportion between two.
/** function syncPage(El, PageView) => Undefined */
function syncPage(el, page){
  const base=el.getBoundingClientRect().top-el.scrollTop;
  /** const byKey: Dict<El> */
  const byKey={};
  for(const w of el.querySelectorAll("[data-wkey]")){
    const k=w.dataset.wkey;
    if(k!==undefined&&byKey[k]===undefined) byKey[k]=w;
  }
  /** const pts: [Number, Number][] */
  const pts=[[0,0]];
  for(const [row,key] of page.marks){
    const w=byKey[key];
    if(w===undefined) continue;
    const top=w.getBoundingClientRect().top-base, last=pts[pts.length-1]??[0,0];
    if(row>last[0]&&top>=last[1]) pts.push([row,top]);
  }
  const tail=pts[pts.length-1]??[0,0];
  pts.push([Math.max(page.content,tail[0]+1), el.scrollHeight]);
  const r=page.offset;
  let i=0; while(i<pts.length-2&&(pts[i+1]??[0,0])[0]<=r) i++;
  const [r0,t0]=pts[i]??[0,0], [r1,t1]=pts[i+1]??[0,0];
  const at=t0+(t1-t0)*Math.min(1,Math.max(0,(r-r0)/Math.max(1,r1-r0)));
  // At the end of the content, the end of the page (heights differ, so the
  // last row of the window lands on the last of the page).
  el.scrollTop=r+page.window>=page.content?el.scrollHeight:at;
}

// Fields stacked under one another line up, as a form's do. The terminal
// pads each inline label (`Search:`, `Files:`) to the cells it needs, so in
// the chrome font their fields start wherever their labels happen to end.
// Labels that start at the same x — a column of them — take the widest one's
// width, so the fields beside them start together.
/** function alignFieldLabels(El) => Undefined */
function alignFieldLabels(root){
  /** const cols: Dict<{l: El, w: Number}[]> */
  const cols={};
  for(const l of root.querySelectorAll(".w-text>.w-text-label,.w-number>.w-text-label,.w-dropdown>.w-text-label")){
    const r=l.getBoundingClientRect();
    if(!r.width) continue;
    const x=String(Math.round(r.left));
    const col=cols[x]??[];
    cols[x]=col;
    col.push({l, w:r.width});
  }
  for(const labels of Object.values(cols)){
    if(labels.length<2) continue;
    const w=labels.reduce((m,lw)=>Math.max(m,lw.w),-Infinity);
    for(const lw of labels) lw.l.style.minWidth=`${w}px`;
  }
}

/** function nativeEls(NativeView) => El[] */
function nativeEls(n){
  const out=[];
  if(!n) return out;
  for(const p of (n.panels||[])){
    // A floating panel stands over everything else. A press outside an
    // anchored pop-up dismisses it (and is spent doing so); a centred modal
    // swallows it. Either way it reaches nothing behind, as in the terminal.
    if(p.kind==="floating"){
      const scrim=div("native-panel-scrim"+(p.anchored?" clear":""));
      scrim.addEventListener("mousedown", e=>{
        e.preventDefault(); e.stopPropagation();
        if(p.anchored) sendWidget({surface:"panel",slot:"floating",eventType:"dismiss"});
      });
      out.push(scrim);
    }
    out.push(nativePanelEl(p));
  }
  const cf=n.confirm;
  if(cf) out.push(...confirmCardEls(cf));
  return out;
}
