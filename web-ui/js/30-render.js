// Per-region DOM patching, motion (FX), render().
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- per-region DOM patching (docs §3.4, the frontend half) ----------------
// Frames arrive as per-region diffs (see applyFrame), so the DOM is patched
// per region too: #app holds one stable display:contents container per region
// family, created once in EXACTLY the order the old monolithic rebuild
// appended elements (display:contents boxes don't exist for layout/stacking,
// so absolute positioning and paint order behave as if every element were
// still a direct child of #app — this changes update granularity, not
// geometry). Rebuilding a region = clearing and refilling only its container.
// Panes get one sub-container per pane index: a typing frame
// ("regions.panes.0") re-emits one pane's SVG and leaves every other DOM node
// untouched — scroll positions, caret blink phase and IME focus survive by
// construction instead of by snapshot/restore.
const REGION_ORDER = ["fileExplorer","panes","separators","menu","statusbar",
  "popups","palette","widgets","contextMenu","auxModal","keybindingEditor",
  "settings","trustDialog","caret"];
let containers=null;
let renderedRegions=[];   // region names rebuilt by the last render (test surface)
function contentsDiv(name){ const d=document.createElement("div"); d.style.display="contents"; d.dataset.region=name; return d; }
function ensureContainers(){
  const app=document.getElementById("app");
  if(containers && containers.caret.isConnected) return;
  app.textContent="";
  containers={};
  for(const n of REGION_ORDER){ const c=contentsDiv(n); app.appendChild(c); containers[n]=c; }
}

// One pane: native tab bar + buffer interior (real cells) + scrollbar (UI).
// The line-number gutter is rendered as its own element, separate from the
// buffer-text flow, so a future native text selection covers only the code
// (not the gutter/line-number column). Both are still real pipeline cells.
function fillPane(c,p){
  if(p.tabBar) c.appendChild(tabBarEl(p));
  const gw=p.gutterWidth||0;
  if(gw>0 && p.gutter){
    // Gutter (line numbers + separator) and buffer text are emitted as
    // separate cell blocks by the bridge — render them as separate elements.
    const g=div("region pane-gutter"); place(g,{x:p.content.x,y:p.content.y,w:gw,h:p.content.h});
    g.innerHTML=cellsSvg(p.gutter, gw); c.appendChild(g);
    const b=div("region pane-content"); place(b,{x:p.content.x+gw,y:p.content.y,w:p.content.w-gw,h:p.content.h});
    b.innerHTML=cellsSvg(p.cells, p.content.w-gw); c.appendChild(b);
  } else {
    const b=div("region pane-content"); place(b,p.content); b.innerHTML=cellsSvg(p.cells, p.content.w); c.appendChild(b);
  }
  if(p.vscroll && p.vscroll.w>0){
    const sb=div("region scrollbar"); place(sb,p.vscroll); c.appendChild(sb);
    const th=document.createElement("div"); th.className="thumb";
    th.style.top=px(p.thumbStart,CH)+"px"; th.style.height=Math.max(CH, px(Math.max(1,p.thumbEnd-p.thumbStart),CH))+"px";
    sb.appendChild(th);
  }
}
function renderPane(i){
  const host=containers.panes;
  if(!host.children[i]){ renderAllPanes(); return; }   // structure drifted → rebuild all
  const c=host.children[i]; c.textContent="";
  const p=scene.regions.panes[i]; if(p) fillPane(c,p);
}
function renderAllPanes(){
  const host=containers.panes, n=scene.regions.panes.length;
  while(host.children.length>n) host.lastChild.remove();
  while(host.children.length<n) host.appendChild(contentsDiv("pane"));
  for(let i=0;i<n;i++){ const c=host.children[i]; c.textContent="";
    const p=scene.regions.panes[i]; if(p) fillPane(c,p); }
}

// Live caret from the pipeline's real cursor cell (if inside a pane). Patched
// in place: when the caret cell didn't move, the existing element is KEPT so
// its blink animation phase doesn't restart on unrelated frames (the old full
// innerHTML rebuild restarted the blink on every frame).
function renderCaret(c,reg){
  let want=null;
  if(reg.cursor){
    const inPane=reg.panes.some(p=>p && reg.cursor.x>=p.content.x && reg.cursor.x<p.content.x+p.content.w && reg.cursor.y>=p.content.y && reg.cursor.y<p.content.y+p.content.h);
    if(inPane) want={left:px(reg.cursor.x,CW)+"px", top:px(reg.cursor.y,CH)+"px",
                     height:CH+"px", width:Math.max(1,Math.round(2*zoom))+"px"};
  }
  const old=c.firstChild;
  if(!want){ if(old) c.textContent=""; return; }
  if(old && old.style.left===want.left && old.style.top===want.top && old.style.height===want.height) return;
  c.textContent="";
  const el=div("caret"); Object.assign(el.style,want); c.appendChild(el);
}

// Region fillers: same element builders the old monolithic render() used,
// now targeted at one container each.
const REGION_FILL={
  // file explorer (UI pane; content = its real rows)
  fileExplorer(c,reg){ if(reg.fileExplorer) c.appendChild(fileExplorerEl(reg.fileExplorer)); },
  panes(){ renderAllPanes(); },
  // split borders
  separators(c,reg){ for(const s of reg.separators){
    const d=div("region separator");
    if(s.vertical) place(d,{x:s.x,y:s.y,w:1,h:s.len}); else place(d,{x:s.x,y:s.y,w:s.len,h:1});
    c.appendChild(d); } },
  // native menu bar + dropdown (one family: menuOpen/menuHighlight/submenuPath/
  // dropdown/menus all redraw together)
  menu(c,reg){ if(reg.menubar) c.appendChild(menuBarEl(reg));
    for(const d of menuDropdownEls(reg)) c.appendChild(d); },
  statusbar(c,reg){ if(reg.statusbar) c.appendChild(statusBarEl(reg.statusbar)); },
  // native popups (completion / hover / action / list / text) — semantic, not cells
  popups(c,reg){ for(const p of (reg.popups||[])) c.appendChild(popupEl(p)); },
  // native command palette / picker (semantic; routes back to the real editor).
  // paletteEls emits the card (with the live-grep / quick-open preview pane
  // rendered inside it, matching the TUI's single box) plus a `.modal-scrim`
  // behind it in centered-modal placement.
  palette(c,reg){ if(reg.palette) for(const el of paletteEls(reg.palette)) c.appendChild(el); },
  // native plugin widget panels (floating / dock)
  widgets(c,reg){ if(reg.widgets) for(const s of reg.widgets) for(const el of widgetSurfaceEls(s)) c.appendChild(el); },
  contextMenu(c,reg){ if(reg.contextMenu) c.appendChild(contextMenuEl(reg.contextMenu)); },
  auxModal(c,reg){ if(reg.auxModal) for(const el of auxModalEls(reg.auxModal)) c.appendChild(el); },
  keybindingEditor(c,reg){ if(reg.keybindingEditor) for(const el of keybindingEditorEls(reg.keybindingEditor)) c.appendChild(el); },
  settings(c,reg){ if(reg.settings) for(const el of settingsEls(reg.settings)) c.appendChild(el); },
  trustDialog(c,reg){ if(reg.trustDialog) for(const el of trustDialogEls(reg.trustDialog)) c.appendChild(el); },
  caret(c,reg){ renderCaret(c,reg); },
};
// COSMOS shell motion (desktop): entrance classes are added only when a
// surface APPEARS — not on the per-keystroke rebuilds of an already-open
// one — and exits animate a positioned clone, since the region rebuild
// removes the real node synchronously.
const FX={
  palette:      {sel:".palette",                present:r=>!!r.palette,        out:"fx-out-pal",  dur:180},
  widgets:      {sel:".widget-surface.w-dock",  present:r=>(r.widgets||[]).some(w=>w.kind==="dock"), out:"fx-out-left", dur:280},
  fileExplorer: {sel:".fileexplorer",           present:r=>!!r.fileExplorer,   out:"fx-out-wipe", dur:280},
};
// Entrance timestamps: a surface often gets a second frame right after it
// appears (focus highlight, async list fill), and that rebuild replaces the
// animating element — without this the enter animation dies after one frame
// and the surface looks like it popped in. The replacement re-joins the
// animation mid-flight via a negative animation-delay.
const fxEnterAt={};
// Workspace switch (the dock's live-switch): everything in the screen changes
// at once, so layout motion is a lie — an explorer that exists in one
// workspace and not the other would appear to slide open/closed. Instead:
// suppress all slides/wipes for a beat and cut with a quick burst of CRT
// static over the screen area.
let curWindowId=null, fxSuppressUntil=0;
const fxReducedMotion=matchMedia("(prefers-reduced-motion: reduce)");
// Quick fade cut over the screen area on a workspace switch — a subtle
// screen-toned veil that fades right out. (The server can settle the new
// workspace's layout in a follow-up frame; that stale-then-snap switch frame
// is a server issue to fix at the source, not something to hide behind a
// longer animation.)
function fxCut(){
  if(isMobile()||fxReducedMotion.matches||!scene) return;
  syncAppOrigin();
  const dw=dockWidthPx();
  const el=div("fx-cut");
  el.style.left=(APPX+dw)+"px"; el.style.top=APPY+"px";
  el.style.width=Math.max(0,px(scene.w||0,CW)-dw)+"px";
  el.style.height=px(scene.h||0,CH)+"px";
  document.body.appendChild(el);
  el.addEventListener("animationend",()=>el.remove());
  setTimeout(()=>{ if(el.isConnected) el.remove(); },400);   // reduced-motion / safety
}
function fxExit(el, cls){
  const r=el.getBoundingClientRect();
  const c=el.cloneNode(true);
  c.classList.remove("fx-in"); c.classList.add("fx-clone", cls);
  c.style.left=r.left+"px"; c.style.top=r.top+"px";
  c.style.width=r.width+"px"; c.style.height=r.height+"px";
  // The clone is pinned at its measured viewport rect, so any transform the
  // class carries (the centered palette's translate(-50%,-50%)) must be
  // neutralised or the clone jumps by half its own size. The exit keyframes
  // override this inline value while they play.
  c.style.transform="none"; c.style.bottom="auto"; c.style.right="auto";
  document.body.appendChild(c);
  c.addEventListener("animationend",()=>c.remove());
  setTimeout(()=>{ if(c.isConnected) c.remove(); },500);   // reduced-motion / safety
}
// FLIP slide for server-driven layout snaps: when the dock or the file
// explorer toggles, every affected screen region re-lands at its new cell
// rect in ONE frame — so the elements are started at a transform that puts
// them back at their OLD position and released to slide into place, matching
// the bezel's own CSS transition (same duration + curve).
function fxTopEls(c, out){
  for(const ch of c.children){
    if(ch.style && ch.style.display==="contents") fxTopEls(ch, out);
    else out.push(ch);
  }
  return out;
}
function fxSlideEls(els, dx){
  if(!dx||!els.length) return;
  const screenR=px((scene&&scene.w)||0,CW);   // grid right edge, #app coords
  const clipped=[];
  for(const el of els){
    el.style.transition="none"; el.style.transform="translateX("+dx+"px)";
    // A rightward start position pokes past the grid's right edge — over the
    // bezel's right rail. Clip the overhang in element space; the inset
    // transitions back to 0 on the same curve as the transform, so the
    // visible boundary eases from the screen edge to the element's own edge
    // and never leaves the screen. (Leftward starts need no clip: the bezel
    // itself is travelling on the same curve, so content stays flush with
    // its moving interior.)
    const l=parseFloat(el.style.left)||0, w=parseFloat(el.style.width)||el.offsetWidth;
    const r0=l+w+dx-screenR;
    if(r0>0){ el.style.clipPath="inset(0 "+r0.toFixed(1)+"px 0 0)"; clipped.push(el); }
  }
  void els[0].offsetWidth;   // commit the start positions before releasing
  for(const el of els){
    el.style.transition="transform .28s cubic-bezier(.2,.8,.25,1), clip-path .28s cubic-bezier(.2,.8,.25,1)";
    el.style.transform="translateX(0)";
  }
  for(const el of clipped) el.style.clipPath="inset(0 0px 0 0)";
  setTimeout(()=>{ for(const el of els){ if(el.isConnected){ el.style.transition=""; el.style.transform=""; el.style.clipPath=""; } } },320);
}
// Cell widths from the last frame; null until the first scene lands. Only a
// PRESENCE change animates — width drags re-layout per pointer frame and
// would fight a 280ms transition.
let fxDockCells=null, fxFeCells=null;
function fxLayoutSlides(){
  if(!scene||!scene.regions||!containers) return;
  let dockC=0;
  for(const s of (scene.regions.widgets||[]))
    if(s.kind==="dock"&&s.rect&&s.rect.x===0&&s.rect.w<(scene.w||0)) dockC=s.rect.w;
  const feC=scene.regions.fileExplorer?scene.regions.fileExplorer.rect.w:0;
  const first=fxDockCells===null;
  const dockDelta=(!first&&(fxDockCells===0)!==(dockC===0))?px(dockC-fxDockCells,CW):0;
  const feDelta  =(!first&&(fxFeCells===0)!==(feC===0))  ?px(feC-fxFeCells,CW):0;
  fxDockCells=dockC; fxFeCells=feC;
  if(isMobile()||(!dockDelta&&!feDelta)||performance.now()<fxSuppressUntil) return;
  const screenEls=[], paneEls=[];
  if(dockDelta) for(const n of ["fileExplorer","menu","statusbar"]) fxTopEls(containers[n],screenEls);
  for(const n of ["panes","separators","caret"]) fxTopEls(containers[n],paneEls);
  fxSlideEls(screenEls,-dockDelta);
  fxSlideEls(paneEls,-(dockDelta+feDelta));
}
function renderRegion(name){
  const c=containers[name];
  // Snapshot scroll positions inside this region before its rebuild, keyed by
  // document-order ordinal (stable across a same-structure re-render).
  const saved=[]; c.querySelectorAll(SCROLL_KEEP).forEach((e,i)=>{ saved[i]=e.scrollTop; });
  const fx=!isMobile()&&performance.now()>=fxSuppressUntil&&FX[name];
  const hadEl=fx?c.querySelector(fx.sel):null;
  const willHave=fx?fx.present(scene.regions):false;
  if(fx&&hadEl&&!willHave){
    fxExit(hadEl, fx.out);
    if(name==="palette"){ const s=c.querySelector(".modal-scrim"); if(s) fxExit(s,"fx-out"); }
  }
  if(name!=="caret") c.textContent="";     // the caret patches its own child in place
  REGION_FILL[name](c, scene.regions);
  if(fx&&willHave){
    const el=c.querySelector(fx.sel);
    if(el&&!hadEl){
      fxEnterAt[name]=performance.now();
      el.classList.add("fx-in");
      if(name==="palette"){ const s=c.querySelector(".modal-scrim"); if(s) s.classList.add("fx-in"); }
    } else if(el&&hadEl){
      const elapsed=performance.now()-(fxEnterAt[name]||0);
      if(elapsed<fx.dur){ el.classList.add("fx-in"); el.style.animationDelay=(-elapsed)+"ms"; }
    }
  }
  c.querySelectorAll(SCROLL_KEEP).forEach((e,i)=>{ if(saved[i]) e.scrollTop=saved[i]; });
  if(name==="settings") settingsEnsureSelectionVisible(c);
  // A widgets rebuild replaces any focused text input — restore DOM focus
  // and caret to the host-focused field (or the hidden sink if none).
  if(name==="widgets") syncWidgetInputFocus();
}

// The TUI windows the settings rows with the core's scroll offset, so its
// selection is always on screen; the web renders the FULL list in native
// overflow:auto panels, and the snapshot/restore above deliberately pins the
// previous scrollTop. When a rebuild moves the selection (arrow keys past the
// fold, and especially a search jump: "/" + query + Enter lands anywhere in
// the tree while the restored scrollTop is from the results view), nudge each
// scrolled panel just enough to bring its selected row back into view. Runs
// after the scroll restore, and only when the row is actually outside the
// panel, so a user's own wheel position is otherwise left alone.
function settingsEnsureSelectionVisible(c){
  for(const sel of c.querySelectorAll(".set-cat.sel,.set-item.sel,.set-sresult.sel,.set-list-row.sel,.set-list-add.sel")){
    const list=sel.closest(".set-cats,.set-items,.set-overlay");
    if(!list) continue;
    const lr=list.getBoundingClientRect(), sr=sel.getBoundingClientRect();
    if(sr.top<lr.top||sr.bottom>lr.bottom) sel.scrollIntoView({block:"nearest"});
  }
}

// Full rebuild of every region container: hello (connect/reconnect), the HTTP
// refresh() resync, and w/h/theme changes (rare). Everything frame-driven goes
// through applyFrame's per-region patch path instead.
function render(){
  const app=document.getElementById("app");
  if(!scene){ containers=null; app.textContent="connecting to editor…"; layoutShell(); return; }
  // Dock live-switch to another workspace: a quick fade cut, plus a
  // suppression window during which the per-region slide/wipe animations
  // stay off — generous (800ms) because the server settles the new
  // workspace's layout in a follow-up frame, which must not slide either.
  const switched=curWindowId!==null&&scene.windowId!==undefined&&scene.windowId!==curWindowId;
  if(scene.windowId!==undefined) curWindowId=scene.windowId;
  if(switched){ fxSuppressUntil=performance.now()+800; fxCut(); }
  applyTheme(scene.theme);
  ensureContainers();
  for(const n of REGION_ORDER) renderRegion(n);
  renderedRegions=REGION_ORDER.slice();
  // Native touch shell on narrow/portrait viewports (desktop is untouched).
  document.body.classList.toggle("mobile", isMobile());
  renderMobileChrome(scene.regions);
  fxLayoutSlides();
  layoutShell();
}

