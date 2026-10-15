// Mobile touch shell + transport (WS frames, resize, actions, clipboard).
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native mobile / portrait touch shell -------------------------------
// Gated by a max-width media query. The header reuses the two hidden top chrome
// rows (36px); the bottom stack's height (M_BOTTOM) is subtracted from the grid
// in resize() so no code hides behind it. Every control routes to the REAL
// editor (sendKey / sendAction), exactly like the desktop chrome.
const M_BASE = 124;                     // px: symbol row + nav + status
const M_MODS_H = 44;                    // px: the optional modifier-key row
const mqMobile = window.matchMedia("(max-width: 640px)");
/** function isMobile() => Boolean */
function isMobile(){ return mqMobile.matches; }
let mSheetOpen = false;                 // overflow (kebab) sheet visibility
let mShowMods = true;                    // show the Termux-style modifier row
// Sticky one-shot modifiers: armed by tapping Ctrl/Alt/Shift, merged into the
// NEXT key (from the symbol row, an arrow, or the device's native keyboard),
// then cleared — like Termux's extra-keys row.
let mMods = { ctrl:false, alt:false, shift:false };
function mBottom(){ return M_BASE + (isMobile() && mShowMods ? M_MODS_H : 0); }

// Hidden text-input sink — the universal text path on mobile AND desktop.
// The page can't toggle the OS keyboard directly, but focusing a hidden input
// summons it (mobile) and gives IMEs a real editable element to compose into
// (desktop CJK input, dead-key accents — whose keydowns arrive as
// "Process"/"Dead" and would otherwise be lost). Plain printable keys,
// shortcuts and named keys still flow through the global keydown handler
// first (which preventDefaults, so the browser never inserts them here);
// only text the keydown path can't express arrives via beforeinput /
// compositionend. The input therefore never accumulates text. On mobile the
// header ⌨ button toggles focus; on desktop it is kept focused permanently
// (no other chrome is focusable). It stays visually hidden but NOT
// display:none — that would break IME composition.
/** let mKbInput: El | Null */
let mKbInput=null;
let mKbWanted=false, kdHandledAt=0;
/** function ensureKbInput() => El */
function ensureKbInput(){
  const have=mKbInput;
  if(have) return have;
  const i=document.createElement("input");
  i.id="m-kbinput"; i.type="text";
  i.setAttribute("autocapitalize","off"); i.setAttribute("autocomplete","off");
  i.setAttribute("autocorrect","off"); i.setAttribute("spellcheck","false");
  i.setAttribute("aria-hidden","true");
  // Android fallback: many soft keyboards report key="Unidentified" on keydown
  // and only deliver the real text via beforeinput. iOS/desktop fire a usable
  // keydown (which cancels its default, normally suppressing this) — so if a
  // keydown was just handled, skip here to avoid double-inserting. Desktop
  // never sets kdHandledAt: preventDefault on keydown already suppresses the
  // matching beforeinput, and arming the 120 ms window there could swallow an
  // IME commit landing right after fast ASCII typing.
  i.addEventListener("beforeinput",ev=>{
    if(ev.isComposing) return;             // IME owns the input until compositionend
    if(Date.now()-kdHandledAt < 120) return;
    const t=ev.inputType||"", data=ev.data; let handled=true;
    if(t==="insertText" && data){ for(const ch of Array.from(data)) mobileSendKey(ch,NO_MODS); }
    else if(t==="insertLineBreak" || t==="insertParagraph") mobileSendKey("Enter",NO_MODS);
    else if(t==="deleteContentBackward") mobileSendKey("Backspace",NO_MODS);
    else if(t==="deleteContentForward") mobileSendKey("Delete",NO_MODS);
    else handled=false;
    if(handled) ev.preventDefault();
    i.value="";
  });
  // Committed IME composition (CJK candidate commit, dead-key accents): the
  // uncommitted preedit lives in this hidden input (inline preedit rendering
  // at the caret is a scene-level design item — see web-ui.md §3.6); on
  // commit, forward the composed text to the editor and clear the sink.
  i.addEventListener("compositionend",ev=>{
    i.value="";
    const composed=ev.data;
    if(composed) for(const ch of Array.from(composed)) mobileSendKey(ch,NO_MODS);
  });
  // Never keep typed text — but don't clear mid-composition, which would
  // cancel the IME's preedit state.
  i.addEventListener("input",ev=>{ if(!ev.isComposing) i.value=""; });
  document.body.appendChild(i); mKbInput=i; return i;
}
// Desktop: keep the sink focused whenever no other focusable chrome needs
// focus (there is none today — chrome is divs). focus({preventScroll}) so it
// can never scroll the page. Mobile keeps its explicit ⌨ toggle instead —
// auto-focusing there would summon the soft keyboard uninvited.
/** function focusSink() => Undefined */
function focusSink(){ if(!isMobile()) ensureKbInput().focus({preventScroll:true}); }
window.addEventListener("focus",()=>focusSink());
mqMobile.addEventListener("change",()=>focusSink());
/** function kbVisible() => Boolean */
function kbVisible(){ return mKbWanted; }
// Toggle driven by an explicit intent flag (not activeElement) so a tap that
// transiently blurs the input can't flip the toggle into re-showing it.
/** function toggleKeyboard() => Undefined */
function toggleKeyboard(){
  const i=ensureKbInput();
  mKbWanted=!mKbWanted;
  if(mKbWanted) i.focus({preventScroll:true}); else i.blur();
  render();   // refresh the ⌨ button's active state
}

// Send a key with the armed sticky modifiers folded in, then disarm them.
/** type KeyMods = {ctrl: Boolean, alt: Boolean, shift: Boolean, meta: Boolean} */
/** const NO_MODS: KeyMods */
const NO_MODS={ ctrl:false, alt:false, shift:false, meta:false };
/** function mobileSendKey(String, KeyMods) => Undefined */
function mobileSendKey(key, d){
  const k={ key,
    ctrl:d.ctrl||mMods.ctrl, alt:d.alt||mMods.alt,
    shift:d.shift||mMods.shift, meta:d.meta };
  const wasArmed = mMods.ctrl||mMods.alt||mMods.shift;
  mMods={ ctrl:false, alt:false, shift:false };
  sendKey(k);
  if(wasArmed) render();                // clear the armed-key highlight at once
}

// Coding-symbol accessory keys (sent as real key events) + Tab; Save = Ctrl+S.
const M_SYMS = ["Tab","{","}","[","]","(",")","<",">",";",":","=","!","&","|","/","\"","'"];
// Termux-style modifier / navigation row. Ctrl/Alt/Shift are sticky toggles
// (a `mod`, no key); the rest are sent immediately (with any armed modifiers).
const M_MODSKEYS = [
  {lb:"Esc", key:"Escape", mod:""}, {lb:"Ctrl", key:"", mod:"ctrl"}, {lb:"Alt", key:"", mod:"alt"}, {lb:"Shift", key:"", mod:"shift"},
  {lb:"←", key:"ArrowLeft", mod:""}, {lb:"↑", key:"ArrowUp", mod:""}, {lb:"↓", key:"ArrowDown", mod:""}, {lb:"→", key:"ArrowRight", mod:""},
  {lb:"Home", key:"Home", mod:""}, {lb:"End", key:"End", mod:""},
];
function modArmed(mod){ return mod==="ctrl"?mMods.ctrl:mod==="alt"?mMods.alt:mMods.shift; }
function toggleMod(mod){
  if(mod==="ctrl") mMods.ctrl=!mMods.ctrl;
  else if(mod==="alt") mMods.alt=!mMods.alt;
  else mMods.shift=!mMods.shift;
}
// Bottom-nav destinations → real editor actions.
const M_NAV = [
  {ic:"🗀", lb:"Files",   act:"toggle_file_explorer"},
  {ic:"⚠", lb:"Issues",  act:"jump_to_next_error"},
  {ic:"❯_",lb:"Console", act:"terminal"},
  {ic:"◈", lb:"LSP",     act:"show_lsp_status"},
  {ic:"⌘", lb:"Palette", act:"command_palette"},
];
// Overflow-menu items → real actions.
const M_MENU = [
  {ic:"⌕", lb:"Go to File…",   act:"quick_open"},
  {ic:"⇄", lb:"Find & Replace", act:"replace"},
  {ic:"❯_",lb:"Terminal",       act:"terminal"},
  {ic:"◈", lb:"LSP Status",     act:"show_lsp_status"},
  {ic:"⚙", lb:"Settings",       act:"open_settings"},
];

// A header button running `fn`.
/** function mobileIcon(String, () => Undefined) => El */
function mobileIcon(glyph, fn){
  const b=div("m-icon"); b.textContent=glyph;
  b.onclick=e=>{ e.stopPropagation(); fn(); };
  return b;
}
/** function renderMobileChrome(Regions) => Undefined */
function renderMobileChrome(reg){
  const host=byId("mobile");
  if(!isMobile()){ if(host.childElementCount) host.innerHTML=""; return; }
  document.documentElement.style.setProperty("--m-bottom", `${mBottom()}px`);
  host.innerHTML="";

  // header: ⚡ Fresh · filename · 🔍 ▶ ⋮
  const hd=div("m-header");
  const logo=div("m-logo"); logo.innerHTML='<span class="m-spark">⚡</span>Fresh'; hd.appendChild(logo);
  const tabs=reg.panes.length?reg.panes[0].tabs:[]; const at=tabs.find(t=>t.active)??(tabs.length?tabs[0]:null);
  const file=div("m-file"); file.textContent=at?(at.label.split("/").pop()??""):"untitled"; hd.appendChild(file);
  hd.appendChild(mobileIcon("🔍",()=>{ mSheetOpen=false; sendAction("search"); }));
  hd.appendChild(mobileIcon("▶",()=>{ mSheetOpen=false; sendAction("command_palette"); }));
  const kb=mobileIcon("⌨",()=>{ mSheetOpen=false; toggleKeyboard(); });
  if(kbVisible()) kb.classList.add("on");
  hd.appendChild(kb);
  hd.appendChild(mobileIcon("⋮",()=>{ mSheetOpen=!mSheetOpen; render(); }));
  host.appendChild(hd);

  if(mSheetOpen){
    const sh=div("m-sheet");
    for(const m of M_MENU){ const it=div("m-sheet-item");
      it.innerHTML='<span class="m-sheet-ic">'+esc(m.ic)+'</span>'+esc(m.lb);
      it.onclick=e=>{ e.stopPropagation(); mSheetOpen=false; sendAction(m.act); }; sh.appendChild(it); }
    // show/hide the Termux-style modifier row (local view preference)
    const tog=div("m-sheet-item"); tog.innerHTML='<span class="m-sheet-ic">'+(mShowMods?"☑":"☐")+'</span>Modifier keys';
    tog.onclick=e=>{ e.stopPropagation(); mShowMods=!mShowMods; mSheetOpen=false; resize(); }; sh.appendChild(tog);
    // centered-modal vs bottom-sheet command palette (local view preference)
    const pct=div("m-sheet-item"); pct.innerHTML='<span class="m-sheet-ic">'+(paletteCentered?"☑":"☐")+'</span>Centered palette';
    pct.onclick=e=>{ e.stopPropagation(); mSheetOpen=false; setPaletteCentered(!paletteCentered); }; sh.appendChild(pct);
    // web-ui theme cycle (local view preference; the desktop pill is hidden on mobile)
    const thm=div("m-sheet-item"); thm.innerHTML='<span class="m-sheet-ic">◐</span>Theme: '+esc(WEB_THEME_LABELS[webTheme]);
    thm.onclick=e=>{ e.stopPropagation(); mSheetOpen=false; cycleWebTheme(1); }; sh.appendChild(thm);
    host.appendChild(sh);
  }

  // bottom stack: [modifier row] · symbols · nav · status
  const bot=div("m-bottom");
  if(mShowMods){
    const mods=div("m-mods");
    for(const m of M_MODSKEYS){
      const armed = m.mod!=="" && modArmed(m.mod);
      const k=div("m-mod"+(m.mod?" m-modkey":"")+(armed?" on":"")); k.textContent=m.lb;
      k.onclick=e=>{ e.stopPropagation();
        if(m.mod){ toggleMod(m.mod); render(); }             // sticky toggle
        else mobileSendKey(m.key,NO_MODS); };                // immediate (with armed mods)
      mods.appendChild(k);
    }
    bot.appendChild(mods);
  }
  const syms=div("m-syms");
  for(const s of M_SYMS){ const k=div("m-sym"); k.textContent=s==="Tab"?"⇥":s;
    k.onclick=e=>{ e.stopPropagation(); mobileSendKey(s,NO_MODS); }; syms.appendChild(k); }
  const save=div("m-sym m-save"); save.textContent="Save";
  save.onclick=e=>{ e.stopPropagation(); mobileSendKey("s",{...NO_MODS,ctrl:true}); }; syms.appendChild(save);
  bot.appendChild(syms);

  const nav=div("m-nav");
  for(const n of M_NAV){ const it=div("m-nav-item");
    it.innerHTML='<span class="m-nav-ic">'+esc(n.ic)+'</span><span class="m-nav-lb">'+esc(n.lb)+'</span>';
    it.onclick=e=>{ e.stopPropagation(); mSheetOpen=false; sendAction(n.act); }; nav.appendChild(it); }
  bot.appendChild(nav);

  const st=div("m-status");
  const sb=reg.statusbar;
  const segs=(sb?sb.segments:[]).filter(s=>s.text!=="");
  for(const s of segs){ const seg=div("m-seg"+(s.name==="trust"?" trust":"")); seg.textContent=s.text; st.appendChild(seg); }
  bot.appendChild(st);
  host.appendChild(bot);
}

// --- bridge to the real editor: WebSocket push transport --------------------
// The server owns the frame loop now (docs/internal/web-ui.md §3.1): it ticks
// the real editor (async LSP/plugin/file events, animations) and PUSHES a
// frame only when the scene actually changed, as region diffs:
//   hello  {type:"hello", seq:0, scene:{...}}          — full scene, resync
//   frame  {type:"frame", seq:N, changed:{path:value}} — path replaces value
// Paths are "w"/"h"/"theme"/"clipboard"/"regions.<key>", with panes diffed
// per index ("regions.panes.<i>" + "regions.panes.len"). Input goes to the
// same socket as JSON with a `type` tag — the exact field shapes the HTTP
// POST bodies always had (those routes still exist for curl / the harness).
// Outbound OS clipboard: the editor copies server-side (its internal
// clipboard; the TUI's OSC 52/arboard push can't reach the browser), so the
// bridge exposes {seq, text} that bumps whenever the copied text changed.
// Writing happens while handling the pushed frame that answers the very
// Ctrl+C / menu click that ran the copy — still within that gesture's
// transient-activation window, which is what navigator.clipboard.writeText
// requires. Absence or rejection (no permission) is silent: the
// editor-internal clipboard still works. The text is never logged.
/** let clipSeq: Number | Null */
let clipSeq=null;
/** function syncClipboard(Clipboard) => Undefined */
function syncClipboard(c){
  if(clipSeq===null){ clipSeq=c.seq; return; }   // boot: don't replay an old copy
  if(c.seq===clipSeq) return;
  clipSeq=c.seq;
  const cb=navigator.clipboard;
  if(c.text && cb) cb.writeText(c.text).catch(()=>{});
}
/** let ws: Socket | Null */
let ws=null;
/** let lastFrameKeys: String[] */
let lastFrameKeys=[];
let frameCount=0, lastSeq=0, wsBackoff=500;
function wsIsOpen(){ const w=ws; return w!==null && w.readyState===1; }
// Send one input message. When the socket is down the input is DROPPED — never
// queued: replaying stale input into a resynced editor is worse than losing a
// keystroke — and the reconnect banner shows.
/** function wsSend<T>(T) => Undefined */
function wsSend(o){ const w=ws; if(w!==null && w.readyState===1){ w.send(JSON.stringify(o)); } else showErr("reconnecting…"); }
// Which region container a "regions.<key>" path dirties. Keys sharing one
// native surface (the menu bar + its dropdown) collapse into one region; null
// means no DOM depends on it (the poll pacing hint); an UNKNOWN key falls back
// to a full render rather than guessing.
// **Every name in `REGION_ORDER` needs an entry here, or its frames fall
// through to a full rebuild.** A path this map does not know is treated as
// unknown rather than as region-scoped (`full=true` below), which is the safe
// default — and a silent one: nothing fails when the two lists drift, the UI
// just quietly gets slower. `tree` drifted exactly that way. It was added to
// `REGION_ORDER` and not here, so every hover over a dock row — one changed
// path, `regions.tree` — cleared and re-filled all fourteen containers,
// re-emitting the buffer's cell SVG among them: 755 of 770 nodes replaced, and
// ~22ms of synchronous work, to recolour one row.
/** const PATH_REGION: Dict<String | Null> */
const PATH_REGION={ fileExplorer:"fileExplorer", separators:"separators",
  menubar:"menu", menus:"menu", menuOpen:"menu", menuHighlight:"menu",
  submenuPath:"menu", dropdown:"menu",
  statusbar:"statusbar", popups:"popups", palette:"palette", widgets:"widgets",
  tree:"tree", native:"native",
  contextMenu:"contextMenu", auxModal:"auxModal",
  keybindingEditor:"keybindingEditor", settings:"settings",
  trustDialog:"trustDialog", cursor:"caret", poll:null };
// Apply a region-diff frame: each changed path replaces its value wholesale
// (null is a legal value — regions are frequently null). Panes are diffed one
// level deeper; "regions.panes.len" truncates/extends the array. Then rebuild
// ONLY the region containers whose paths changed (w/h/theme → full rebuild;
// they re-place every rect / re-seed the CSS vars, and they're rare).
// A path names its value's type only as a string, so each value is given the
// type of the field it replaces (`jsonAs`, unchecked, like `JSON.parse`).
/** function setRegion(Regions, String, FrameValue) => Undefined */
function setRegion(r, key, v){
  switch(key){
    case "menubar": r.menubar=jsonAs(v); break;
    case "menus": r.menus=jsonAs(v); break;
    case "menuOpen": r.menuOpen=jsonAs(v); break;
    case "menuHighlight": r.menuHighlight=jsonAs(v); break;
    case "submenuPath": r.submenuPath=jsonAs(v); break;
    case "dropdown": r.dropdown=jsonAs(v); break;
    case "statusbar": r.statusbar=jsonAs(v); break;
    case "fileExplorer": r.fileExplorer=jsonAs(v); break;
    case "panes": r.panes=jsonAs(v); break;
    case "separators": r.separators=jsonAs(v); break;
    case "popups": r.popups=jsonAs(v); break;
    case "tree": r.tree=jsonAs(v); break;
    case "native": r.native=jsonAs(v); break;
    case "palette": r.palette=jsonAs(v); break;
    case "trustDialog": r.trustDialog=jsonAs(v); break;
    case "contextMenu": r.contextMenu=jsonAs(v); break;
    case "auxModal": r.auxModal=jsonAs(v); break;
    case "keybindingEditor": r.keybindingEditor=jsonAs(v); break;
    case "settings": r.settings=jsonAs(v); break;
    case "cursor": r.cursor=jsonAs(v); break;
    case "poll": r.poll=jsonAs(v); break;
  }
}
/** function applyFrame(Dict<FrameValue>) => Undefined */
function applyFrame(changed){
  const sc=liveScene();   // frames only follow a hello
  let clipChanged=false, full=false;
  /** const dirty: String[] */
  const dirty=[];
  const mark=name=>{ if(!dirty.includes(name)) dirty.push(name); };
  for(const [k, v] of Object.entries(changed)){
    if(k==="w"){ sc.w=jsonAs(v); full=true; }
    else if(k==="h"){ sc.h=jsonAs(v); full=true; }
    else if(k==="theme"){ sc.theme=jsonAs(v); full=true; }   // applyTheme runs in render()
    else if(k==="windowId"){ sc.windowId=jsonAs(v); full=true; }   // workspace switch → hard cut (see render)
    else if(k==="clipboard"){ sc.clipboard=jsonAs(v); clipChanged=true; }
    else if(k==="regions.panes.len"){ sc.regions.panes.length=jsonAs(v); mark("panes"); mark("caret"); }
    else if(k.startsWith("regions.panes.")){ sc.regions.panes[Number(k.slice(14))]=jsonAs(v);
      mark("panes."+k.slice(14));
      mark("caret"); }   // caret geometry gates on pane rects (renderCaret)
    else if(k.startsWith("regions.")){ const key=k.slice(8); setRegion(sc.regions, key, v);
      const r=PATH_REGION[key]; if(r) mark(r); else if(r===undefined) full=true; }
    else full=true;
  }
  if(clipChanged) syncClipboard(sc.clipboard);
  if(full || !containers){ render(); return; }
  const todo=dirty.includes("panes")?dirty.filter(d=>!d.startsWith("panes.")):dirty;
  renderedRegions=[];
  for(const name of todo){
    if(name==="panes") renderAllPanes();
    else if(name.startsWith("panes.")) renderPane(Number(name.slice(6)));
    else renderRegion(name);
    renderedRegions.push(name);
  }
  // The mobile chrome mirrors tabs + status segments; rebuilding it is cheap,
  // so any frame refreshes it (exactly what the old full rebuild always did;
  // on desktop renderMobileChrome returns immediately).
  document.body.classList.toggle("mobile", isMobile());
  renderMobileChrome(sc.regions);
  syncMacTitle();
  fxLayoutSlides();
  layoutShell();   // dock width / grid size may have moved the bezel
}
// A message from the server, or null for one that isn't JSON.
/** function parseServerMessage(String) => ServerMessage | Null */
function parseServerMessage(text){ try{ return JSON.parse(text); }catch(_){ return null; } }
function connect(){
  const sock=new WebSocket((location.protocol==="https:"?"wss":"ws")+"://"+location.host+"/ws");
  ws=sock;
  sock.onmessage=ev=>{
    const m=parseServerMessage(ev.data);
    if(!m) return;
    if(m.type==="hello"){
      // Full resync: fresh connect, reconnect, or a restarted server all land
      // here. Adopt the clipboard seq WITHOUT writing — replaying a copy from
      // before the reconnect would be stale.
      wsBackoff=500; hideErr(); hideReconnect();
      const sc=m.scene;
      scene=sc; lastSeq=m.seq;
      if(sc.clipboard) clipSeq=sc.clipboard.seq;
      render();
      resize();     // fit the grid to this window; the server pushes the resized frame
    } else {   // "frame"
      frameCount++; lastSeq=m.seq; lastFrameKeys=Object.keys(m.changed);
      applyFrame(m.changed);
    }
  };
  // Close/error → non-modal banner + retry with backoff (500 ms doubling to
  // 5 s max). The hello on reconnect fully resyncs (the server state survived;
  // a server restart lands here too); input meanwhile is dropped. Every client
  // mirrors the same editor, so a reconnect just rejoins the shared session —
  // there is no single slot to be locked out of.
  sock.onclose=()=>{ showReconnect(); const d=wsBackoff; wsBackoff=Math.min(5000,wsBackoff*2); setTimeout(connect,d); };
}
// Full-scene resync over the HTTP /state route (which stays curl-able for the
// parity harness). Rarely needed — hello/frames carry all state — but kept
// working for tests and manual recovery.
async function refresh(){ try{ /** const s: Scene */ const s=await (await fetch("/state")).json(); scene=s; render(); }catch(e){ showErr("load failed: "+String(e)); } }
/** function sendKey<T>(T) => Undefined */
function sendKey(d){ wsSend({type:"key",...d}); }
function sendPaste(text){ wsSend({type:"paste",text}); }
/** function resize() => Undefined */
function resize(){ const a=byId("app");
  // Fit the grid to #app's box (the window minus the COSMOS shell's wallpaper
  // margin + bezel insets; full-bleed again on mobile).
  const cols=Math.max(20,Math.floor(a.clientWidth/CW));
  // On mobile, leave room for the fixed bottom stack so no code hides behind it
  // (the header reuses the two hidden top chrome rows, so it needs no reserve).
  const availH=a.clientHeight - (isMobile()?mBottom():0);
  const rows=Math.max(8,Math.floor(availH/CH));
  wsSend({type:"resize",cols,rows}); }
/** function sendAction(String) => Undefined */
function sendAction(name){ mSheetOpen=false; wsSend({type:"action",action:name}); }
function showErr(m){ const e=byId("err"); e.textContent=m; e.style.display="block"; }
function hideErr(){ const e=byId("err"); e.style.display="none"; }
// The WS reconnect state is a client concern (the scene projection can't know
// the socket dropped), so it lives in its own fixed-corner pill instead of the
// #err banner (which stays reserved for genuine errors). Shown while the socket
// is down, cleared on the reconnect `hello`.
function showReconnect(){ byId("reconnect").classList.add("show"); }
function hideReconnect(){ byId("reconnect").classList.remove("show"); }

const NAMED=new Set(["Enter","Backspace","Delete","Tab","Escape","ArrowUp","ArrowDown","ArrowLeft","ArrowRight","Home","End","PageUp","PageDown"]);
document.addEventListener("keydown",e=>{
  // IME composition in progress: the keys belong to the IME (key is usually
  // "Process"); the committed text arrives via the hidden input's
  // compositionend. Forwarding here would double-deliver on browsers that
  // report the raw key during composition.
  if(e.isComposing) return;
  // App zoom: Ctrl/Cmd + = / - / 0 are FRONTEND-OWNED and never reach the
  // editor — like the browser's own zoom they change the view's typography
  // only; the editor just sees the resulting cols/rows re-fit (resize).
  // Anyone rebinding these in the editor must pick different chords on the
  // web. (Ctrl+wheel zoom is intercepted the same way below.)
  if((e.ctrlKey||e.metaKey) && !e.altKey &&
     (e.key==="="||e.key==="+"||e.key==="-"||e.key==="_"||e.key==="0")){
    e.preventDefault();
    setZoom(e.key==="0" ? 1 : zoom + ((e.key==="-"||e.key==="_") ? -0.1 : 0.1));
    return;
  }
  // Palette placement toggle: Ctrl/Cmd+Alt+P is FRONTEND-OWNED (like zoom) —
  // it flips the command palette between the centered modal and the bottom
  // sheet and never reaches the editor. Alt distinguishes it from the editor's
  // own Ctrl/Cmd+P (quick open), which still forwards normally.
  if((e.ctrlKey||e.metaKey) && e.altKey && (e.key==="p"||e.key==="P"||e.code==="KeyP")){
    e.preventDefault();
    setPaletteCentered(!paletteCentered);
    return;
  }
  // Alt-selection toggle: Ctrl/Cmd+Alt+S is FRONTEND-OWNED (like the two
  // above) — it enables/disables the hold-Alt native text-selection mode
  // for users whose workflow clashes with Alt-drag, persisted in
  // localStorage. Never reaches the editor.
  if((e.ctrlKey||e.metaKey) && e.altKey && (e.key==="s"||e.key==="S"||e.code==="KeyS")){
    e.preventDefault();
    setNatselEnabled(!natselEnabled);
    return;
  }
  // Web-theme cycle: Ctrl/Cmd+Alt+T is FRONTEND-OWNED (like the three above) —
  // it steps the web UI theme (Cosmos → macOS → Compact) and never reaches the
  // editor. Shift reverses the direction.
  if((e.ctrlKey||e.metaKey) && e.altKey && (e.key==="t"||e.key==="T"||e.code==="KeyT")){
    e.preventDefault();
    cycleWebTheme(e.shiftKey?-1:1);
    return;
  }
  // forward single characters (with or without modifiers, so Ctrl+P / Ctrl+S
  // shortcuts reach the editor) and the named navigation/edit keys.
  const single=e.key.length===1;
  if(single||NAMED.has(e.key)){
    // Ctrl/Cmd+V: don't forward and don't preventDefault — the browser will
    // fire a DOM 'paste' event carrying the OS clipboard, and that event is
    // the web UI's one paste path (see the document 'paste' listener).
    // Forwarding the bare key as well would ALSO run the editor's internal
    // paste (double paste). This mirrors the terminal frontend, where paste
    // arrives as a bracketed-paste event, never as a Ctrl+V keystroke.
    if(single&&(e.ctrlKey||e.metaKey)&&!e.altKey&&e.key.toLowerCase()==="v") return;
    // Ctrl/Cmd+C with a live NATIVE selection (Alt+drag, see natsel below):
    // copy that selection instead of forwarding the key — the editor knows
    // nothing about it (it may cover a terminal or chrome). Row-aware SVG
    // extraction keeps multi-line copies multi-line. Alt is deliberately
    // NOT excluded: the natural gesture is Ctrl+C while Alt is still held
    // from the selection drag. With no native selection the key forwards
    // as before (editor-internal copy + the scene clipboard sync).
    if(single&&(e.ctrlKey||e.metaKey)&&e.key.toLowerCase()==="c"){
      const t=natselText();
      if(t){ e.preventDefault();
        const cb=navigator.clipboard;
        if(cb) cb.writeText(t).catch(()=>{});
        return; }
    }
    e.preventDefault();
    // `code` is the PHYSICAL key (e.g. "Digit1" for the key that types '!'
    // with Shift). The bridge needs it because bindings are written against
    // the unshifted key plus SHIFT, the way a terminal reports them.
    const d={key:e.key,code:e.code,ctrl:e.ctrlKey,meta:e.metaKey,shift:e.shiftKey,alt:e.altKey};
    // On mobile, fold in any armed sticky modifiers so e.g. tap-Ctrl then a
    // native-keyboard letter sends Ctrl+<letter>. Record the time so the
    // beforeinput fallback (Android) knows this key was already handled.
    if(isMobile()){ kdHandledAt=Date.now(); mobileSendKey(d.key,{ctrl:d.ctrl,alt:d.alt,shift:d.shift,meta:d.meta}); } else sendKey(d);
  }
});
// Inbound OS clipboard: the DOM paste event (Ctrl+V / Shift+Insert / browser
// menu) carries the system clipboard, which the server can't read. One POST
// /paste delivers the whole text through the editor's real bracketed-paste
// path — never per-char /key loops.
document.addEventListener("paste",e=>{
  e.preventDefault();
  const cd=e.clipboardData;
  const text=cd?cd.getData("text/plain"):"";
  if(text) sendPaste(text);
});
