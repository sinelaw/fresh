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
function nativePanelsOn(){
  let pref=null;
  try { pref=localStorage.getItem("fresh.nativePanels"); } catch(_) {}
  if(pref==="on") return true;
  if(pref==="off") return false;
  return NATIVE_PANEL_THEMES.includes(webTheme);
}

function sendNode(key, action){ wsSend({type:"node", key, action:action||"press"}); }

// Accelerators are underlined only while Alt is held, as native dialogs do;
// the letters still answer without it (the editor's confirm keys).
for(const t of ["keydown","keyup"]) document.addEventListener(t, e=>{
  if(e.key==="Alt") document.body.classList.toggle("alt-held", t==="keydown");
}, true);
window.addEventListener("blur", ()=>document.body.classList.remove("alt-held"));

// A control that never takes the keyboard: the editor owns every key, and a
// focused <button> would answer Enter/Space itself as well. Pressing happens
// on release over the control, as with any native button.
function nativeButton(cls, key){
  const b=document.createElement("button");
  b.type="button"; b.className=cls; b.tabIndex=-1;
  b.addEventListener("mousedown", e=>{ e.preventDefault(); e.stopPropagation(); });
  b.addEventListener("click", e=>{ e.preventDefault(); e.stopPropagation(); sendNode(key); });
  return b;
}

// ---- the confirmation card ----------------------------------------------
function confirmCardEls(c){
  const scrim=div("native-scrim");
  // The card is modal: a press outside it answers nothing (the terminal's
  // layer swallows it the same way).
  scrim.addEventListener("mousedown", e=>{ e.preventDefault(); e.stopPropagation(); });
  const card=div("native-card native-confirm");
  card.setAttribute("role","alertdialog");
  card.addEventListener("mousedown", e=>{ e.stopPropagation(); });
  const title=div("nc-title"); title.textContent=c.title; card.appendChild(title);
  if(c.body){ const p=div("nc-body"); p.textContent=c.body; card.appendChild(p); }
  if(c.detail){ const d=div("nc-detail"); d.textContent=c.detail; card.appendChild(d); }
  const row=div("nc-buttons");
  for(const b of c.buttons){
    const el=nativeButton("nc-btn"+(b.selected?" selected":"")+(b.destructive?" destructive":"")
      +(b.escape?" escape":""), b.key);
    const [pre, acc, post]=b.label;
    el.append(pre);
    if(acc){ const u=document.createElement("span"); u.className="accel"; u.textContent=acc; el.append(u); }
    if(post) el.append(post);
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
function nativePanelEl(p){
  // A floating panel is a card over its whole frame — its title and close box
  // drawn here too — with the interior laid out in what is left of it.
  const framed=p.kind==="floating"&&p.frame;
  const el=div("native-panel np-"+p.kind+(p.keyboard?" kb":"")+(framed?" card":"")+(p.anchored?" anchored":""));
  place(el, framed?p.frame:p.rect);
  // An anchored pop-up hugs its content, as the terminal's does — natively
  // laid out, that content is not the cells the terminal measured, so the
  // card takes its height (and any extra width) from what it holds.
  if(framed&&p.anchored){
    el.style.height="auto";
    el.style.minWidth=el.style.width; el.style.width="max-content";
    el.style.maxHeight="calc(100% - "+el.style.top+" - 4px)";
  }
  el.dataset.slot=p.kind;
  let body=el;
  if(framed){
    if(p.title||p.closeKey){
      const head=div("np-head");
      const t=div("np-title"); t.textContent=p.title||""; head.appendChild(t);
      if(p.closeKey){
        const x=nativeButton("np-close", p.closeKey);
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
  const ctx={kind:"panel", slot:p.kind, section:p.section, focusKey:p.focusKey,
    instances:p.instances||{}, native:true};
  body.appendChild(widgetEl(p.spec, ctx));
  // Keep the selected row of a list or tree in view as the keyboard moves it:
  // the page scrolls its own lists.
  requestAnimationFrame(()=>{
    for(const s of el.querySelectorAll(".w-list-row.sel,.w-list-card.sel,.w-tree-row.sel")){
      s.scrollIntoView({block:"nearest"});
    }
  });
  return el;
}

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
  if(n.confirm) out.push(...confirmCardEls(n.confirm));
  return out;
}
