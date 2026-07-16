// Plugin widget tree (WidgetSpec) + Settings / keybinding editor / aux modals.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native plugin widgets (WidgetSpec tree) ----------------------------
// Renders the serialized WidgetSpec natively (Row/Col→flex, Toggle/Button→
// native controls, …). Toggle/Button clicks route back to the owning plugin
// via the same event the TUI fires; `surface` selects the routing path.
// Concatenate a TextPropertyEntry's display text (segments preferred).
function entryText(e){ return (e.segments&&e.segments.length)?e.segments.map(s=>s.text||"").join(""):(e.text||""); }

// Route a widget interaction. `ctx.kind` is "toolbar" (prompt toolbar, routed by
// key) or "panel" (floating/dock, routed by the hit's IDENTITY — widgetKey +
// eventType + payload — with the recorded hit's index as a legacy tiebreaker).
// Raw indices alone are fragile: the editor regenerates the hits list on every
// panel re-render (a click can race a frame), and list hits are WINDOWED to
// the TUI's visible rows — this natively-scrolled frontend renders the whole
// list, so a row below the TUI fold has no recorded hit at all. Identity
// delivery survives both (the bridge resolves by equality, and synthesizes
// off-window list selects from the panel's own spec).
function routeWidget(ctx, node, listIndex, ev){
  if(ctx.kind==="toolbar"){ if(node.key) sendWidget({surface:"toolbar",key:node.key}); return; }
  const base={surface:"panel",plugin:ctx.plugin,panelId:ctx.panelId};
  if(listIndex!=null){
    // List row/card click: match the recorded hit by row index AND list key
    // (a panel can hold several lists), fall back to the spec's own itemKeys
    // for rows outside the TUI scroll window.
    const hit=(ctx.hits||[]).find(h=>h.widgetKind==="list" && h.payload && h.payload.index===listIndex
      && (!node.key || h.payload.list_key===node.key));
    const itemKey=(node.itemKeys&&node.itemKeys[listIndex]!=null)?node.itemKeys[listIndex]:(hit?hit.payload.key:"");
    // Right-click fires `context` (never recorded as a hit — the bridge
    // synthesizes it, like the TUI's right-click path) with the click cell
    // so the plugin can anchor its context-menu popup.
    if(ev&&ev.button===2){
      const c=cellAt(ev);
      sendWidget({...base,widgetKey:itemKey||"",eventType:"context",
        payload:{index:listIndex,key:itemKey||"",list_key:node.key||null,col:c.col,row:c.row}});
      return;
    }
    const msg={...base,widgetKey:itemKey||"",eventType:"select",
      payload:{index:listIndex,key:itemKey||"",list_key:node.key||null}};
    if(hit) msg.hitIndex=hit.index;
    sendWidget(msg);
    return;
  }
  if(node.key){
    const hit=(ctx.hits||[]).find(h=>h.widgetKey===node.key);
    if(hit){ sendWidget({...base,widgetKey:node.key,eventType:hit.eventType,payload:hit.payload,hitIndex:hit.index}); return; }
    // No recorded hit: the TUI clipped this control below the panel's inner
    // rect, but the DOM grew the panel to fit and rendered it anyway. Send
    // the identity with the kind's own event — the bridge synthesizes the
    // hit from the panel's spec (state like `checked`/`disabled` is read
    // from the spec server-side, not trusted from here).
    const ev=node.kind==="button"?"activate":node.kind==="toggle"?"toggle":node.kind==="text"?"focus":null;
    if(ev) sendWidget({...base,widgetKey:node.key,eventType:ev,payload:{}});
  }
}

// Route a control interaction by key + SPECIFIC eventType. routeWidget's
// keyed path picks the first hit with a matching key, which is ambiguous
// for widgets that record several hit kinds under one key (a dropdown
// records `dropdown_toggle` plus one `dropdown_select` per option row).
// Falls back to sending the bare identity; the bridge synthesizes the hit
// from the panel's spec (same as the clipped-control path).
function routeControl(ctx,key,eventType,payload){
  const base={surface:"panel",plugin:ctx.plugin,panelId:ctx.panelId};
  const hit=(ctx.hits||[]).find(h=>h.widgetKey===key&&h.eventType===eventType
    &&(!(payload&&payload.index!=null)||(h.payload&&h.payload.index===payload.index)));
  const msg={...base,widgetKey:key,eventType,payload:hit?hit.payload:(payload||{})};
  if(hit) msg.hitIndex=hit.index;
  sendWidget(msg);
}

// UTF-8 byte offset ⇄ UTF-16 code-unit index conversions for text widgets:
// the host TextEdit speaks bytes, input.selectionStart speaks UTF-16.
function byteToUtf16(s,byte){
  let b=0;
  for(let i=0;i<s.length;i++){
    if(b>=byte) return i;
    const c=s.codePointAt(i);
    b+=c<0x80?1:c<0x800?2:c<0x10000?3:4;
    if(c>0xffff) i++;
  }
  return s.length;
}
function utf16ToByte(s,idx){
  let b=0;
  for(let i=0;i<Math.min(idx,s.length);i++){
    const c=s.codePointAt(i);
    b+=c<0x80?1:c<0x800?2:c<0x10000?3:4;
    if(c>0xffff) i++;
  }
  return b;
}

// After a widgets-region rebuild: hand real DOM focus to the host-focused
// text widget's input (so the native caret blinks and IME composes there),
// pin its caret to the host TextEdit's cursor, and fall back to the hidden
// sink when no widget input holds focus anymore. Skipped mid-IME-composition
// so a caret write can't cancel the preedit.
function syncWidgetInputFocus(){
  const t=document.querySelector(".w-text-input[data-wfocus='1']");
  if(t){
    if(document.activeElement!==t) t.focus({preventScroll:true});
    if(!t.dataset.composing&&t.dataset.caret!==""&&t.dataset.caret!=null){
      const i=+t.dataset.caret;
      try{ t.setSelectionRange(i,i); }catch(_){}
    }
  } else if(document.activeElement&&document.activeElement.classList
      &&document.activeElement.classList.contains("w-text-input")){
    focusSink();
  }
}

function widgetEl(spec, ctx){
  if(!spec) return document.createTextNode("");
  const kind=spec.kind, fk=ctx.focusKey;
  const focused = spec.key && spec.key===fk;
  if(kind==="row"||kind==="col"){
    const el=div("w-"+kind+(spec.wrap?" wrap":""));
    for(const c of (spec.children||[])) el.appendChild(widgetEl(c, ctx));
    return el;
  }
  if(kind==="toggle"){
    const el=div("w-toggle"+(spec.checked?" on":"")+(focused?" focus":""));
    // A sliding switch (CSS-drawn), matching the Settings UI toggles —
    // not a unicode ☑/☐ glyph.
    const box=document.createElement("span"); box.className="w-box"; el.appendChild(box);
    const lb=document.createElement("span"); lb.className="w-label"; lb.textContent=spec.label||""; el.appendChild(lb);
    if(spec.key) el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec); };
    return el;
  }
  if(kind==="button"){
    const el=div("w-button "+(spec.intent||"normal")+(spec.disabled?" disabled":"")+(focused?" focus":""));
    el.textContent=spec.label||"";
    if(spec.key && !spec.disabled) el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec); };
    return el;
  }
  if(kind==="spacer"){ const el=div("w-spacer"); if(spec.flex) el.style.flex="1"; else el.style.width=((spec.cols||1)*CW)+"px"; return el; }
  if(kind==="divider"){ return div("w-divider"); }
  if(kind==="hintBar"){
    const el=div("w-hintbar");
    for(const h of (spec.entries||[])){ const e2=document.createElement("span"); e2.className="w-hint";
      e2.innerHTML='<b>'+esc(h.keys||"")+'</b> '+esc(h.label||""); el.appendChild(e2); }
    return el;
  }
  if(kind==="labeledSection"){
    const el=div("w-section"); if(spec.label){ const l=div("w-section-label"); l.textContent=spec.label; el.appendChild(l); }
    if(spec.child) el.appendChild(widgetEl(spec.child, ctx)); return el;
  }
  if(kind==="text"){
    // A REAL <input>/<textarea>, but the editor stays the single source of
    // truth: the host TextEdit's live value + caret arrive via instance
    // state (instances[key].textValue / cursorByte) and are written into
    // the element on every frame. Keystrokes never edit the element
    // locally — the global keydown handler preventDefaults and forwards
    // them, and the pushed frame echoes the authoritative result back.
    // The native element buys the real caret, focus ring, IME target and
    // browser-computed click-to-caret positions for free.
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const val=(inst&&inst.textValue!=null)?inst.textValue:(spec.value||"");
    const el=div("w-text"+(focused?" focus":""));
    if(spec.label){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=spec.label+": "; el.appendChild(l); }
    const multi=(spec.rows||1)>1;
    const input=document.createElement(multi?"textarea":"input");
    input.className="w-text-input";
    if(multi) input.rows=spec.rows;
    input.value=val;
    input.placeholder=spec.placeholder||"";
    input.setAttribute("autocapitalize","off"); input.setAttribute("autocomplete","off");
    input.setAttribute("autocorrect","off"); input.setAttribute("spellcheck","false");
    // Block every local edit outside the key-forwarding path (autofill,
    // drag-drop text, context-menu paste): the paste event still bubbles to
    // the document paste listener (the one real paste path), and IME
    // composition — which beforeinput can't cancel — is forwarded on commit
    // exactly like the hidden sink does, then discarded locally.
    input.addEventListener("beforeinput",ev=>{ if(!ev.isComposing) ev.preventDefault(); });
    input.addEventListener("compositionstart",()=>{ input.dataset.composing="1"; });
    input.addEventListener("compositionend",ev=>{ delete input.dataset.composing; if(ev.data) sendPaste(ev.data); input.value=val; });
    input.addEventListener("input",ev=>{ if(!ev.isComposing) input.value=val; });
    // Mousedown focuses the widget host-side (the same `focus` event a TUI
    // click fires). No preventDefault — the browser places its caret
    // natively, and mouseup reports that position so the host TextEdit
    // follows. (A drag-selection is left as a browser-local convenience
    // for copy; the host tracks selection only via its own keybindings.)
    input.addEventListener("mousedown",e=>{ e.stopPropagation(); if(spec.key) routeWidget(ctx,spec); });
    input.addEventListener("mouseup",e=>{ e.stopPropagation();
      if(spec.key && input.selectionStart===input.selectionEnd)
        sendWidget({surface:"panel",plugin:ctx.plugin,panelId:ctx.panelId,widgetKey:spec.key,
          textCursor:utf16ToByte(input.value,input.selectionStart)});
    });
    if(focused){
      input.dataset.wfocus="1";
      input.dataset.caret=(inst&&inst.cursorByte!=null)?String(byteToUtf16(val,inst.cursorByte)):"";
    }
    el.appendChild(input);
    // Completion popup (e.g. path complete): candidates are host instance
    // state; the highlighted row appears only once ↑/↓ entered the popup,
    // mirroring the TUI.
    if(focused&&inst&&inst.completions&&inst.completions.length){
      const dd=div("w-complete");
      inst.completions.forEach((cand,i)=>{
        const r=div("w-complete-row"+((inst.completionNavigated&&i===(inst.completionSelected||0))?" sel":""));
        r.textContent=cand; dd.appendChild(r);
      });
      el.appendChild(dd);
    }
    return el;
  }
  if(kind==="number"){
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const val=(inst&&inst.numberValue!=null)?inst.numberValue:(spec.value||0);
    const el=div("w-number"+(focused?" focus":""));
    if(spec.label){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=spec.label+": "; el.appendChild(l); }
    const disp=spec.editText!=null?spec.editText
      :spec.percent?Math.round(val*100)+"%"
      :spec.integer?String(Math.trunc(val))
      :String(+(+val).toFixed(3));
    // −/+ steppers focus the widget then step it via the host's own
    // Left/Right handling — the value math (min/max/step) stays host-side.
    const mk=(t,key)=>{ const b=document.createElement("span"); b.className="w-num-step"; b.textContent=t;
      b.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(!spec.key) return;
        routeControl(ctx,spec.key,"number_value",{}); sendKey({key}); };
      return b; };
    el.appendChild(mk("−","ArrowLeft"));
    const v=document.createElement("span"); v.className="w-num-val"; v.textContent=disp;
    v.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"number_value",{}); };
    el.appendChild(v);
    el.appendChild(mk("+","ArrowRight"));
    return el;
  }
  if(kind==="dropdown"){
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const selIdx=(inst&&inst.selectedIndex!=null)?inst.selectedIndex:(spec.selectedIndex||0);
    const open=inst?!!inst.dropdownOpen:!!spec.open;
    const el=div("w-dropdown"+(focused?" focus":""));
    if(spec.label){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=spec.label+": "; el.appendChild(l); }
    const pill=document.createElement("span"); pill.className="w-dd-pill";
    pill.textContent=((spec.options||[])[selIdx]??"—")+(open?" ▲":" ▾");
    pill.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"dropdown_toggle",{}); };
    el.appendChild(pill);
    if(open){
      const dd=div("w-dd");
      (spec.options||[]).forEach((o,i)=>{
        const r=div("w-dd-row"+(i===selIdx?" sel":"")); r.textContent=o;
        r.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"dropdown_select",{index:i}); };
        dd.appendChild(r);
      });
      el.appendChild(dd);
    }
    return el;
  }
  if(kind==="dualList"){
    // Two-column ordered-subset picker. Rows are derived exactly like the
    // host: Available = options minus included minus excluded (in option
    // order); Included = the host-owned ordered instance list. Clicks fire
    // the same `dual_focus` hits a TUI cell click resolves; moves/reorders
    // stay keyboard-driven through the host (Space / PageUp / PageDown).
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    // `included` is skip-serialized when empty, so detect "host state
    // exists" via activeIncluded (always present for a DualList instance).
    const included=(inst&&inst.activeIncluded!=null)?(inst.included||[]):(spec.included||[]);
    const activeIncluded=inst?!!inst.activeIncluded:false;
    const cursors={available:inst&&inst.availableCursor||0, included:inst&&inst.includedCursor||0};
    const optLabel=v=>{ const o=(spec.options||[]).find(o=>(o.value??o)===v); return o?(o.label??o.value??o):v; };
    const excluded=new Set([...(spec.excluded||[]),...included]);
    const avail=(spec.options||[]).map(o=>o.value??o).filter(v=>!excluded.has(v));
    const el=div("w-dual"+(focused?" focus":""));
    if(spec.label){ const l=div("w-section-label"); l.textContent=spec.label; el.appendChild(l); }
    const cols=div("w-dual-cols");
    const mkCol=(title,values,column,active)=>{
      const c=div("w-dual-col"+(active?" active":""));
      const t=div("w-dual-title"); t.textContent=title; c.appendChild(t);
      values.forEach((v,i)=>{
        const r=div("w-dual-row"+(active&&i===cursors[column]?" sel":"")); r.textContent=optLabel(v);
        r.onmousedown=e=>{ e.preventDefault(); e.stopPropagation();
          if(spec.key) routeControl(ctx,spec.key,"dual_focus",{column,index:i}); };
        c.appendChild(r);
      });
      return c;
    };
    cols.appendChild(mkCol("Available",avail,"available",!activeIncluded));
    cols.appendChild(mkCol("Included",included,"included",activeIncluded));
    el.appendChild(cols);
    return el;
  }
  if(kind==="list"){
    const el=div("w-list");
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const sel=inst?inst.selectedIndex:(spec.selectedIndex!=null?spec.selectedIndex:-1);
    const specs=spec.itemSpecs||[];
    if(specs.length){
      // Card list: each logical item is a WidgetSpec (e.g. a LabeledSection
      // "card"); the whole card is one select hit. Overrides `items`.
      el.classList.add("w-list-cards");
      specs.forEach((s,i)=>{
        const card=div("w-list-card"+(i===sel?" sel":""));
        card.appendChild(widgetEl(s, ctx));
        card.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec,i,e); };
        el.appendChild(card);
      });
    } else {
      (spec.items||[]).forEach((it,i)=>{
        const row=div("w-list-row"+(i===sel?" sel":""));
        row.textContent=entryText(it);
        row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec,i,e); };
        el.appendChild(row);
      });
      if(!(spec.items||[]).length){ const empty=div("w-list-empty"); empty.textContent="(empty)"; el.appendChild(empty); }
    }
    return el;
  }
  if(kind==="tree"){
    // Same semantics as the TUI's render_widget_tree: the host-owned
    // instance state (selection, expanded set) is authoritative, a node is
    // visible iff every ancestor is expanded, and rows fire the identical
    // select / expand hits. Rendered natively (all visible rows, no TUI
    // scroll window) — off-window clicks are synthesized bridge-side.
    const el=div("w-tree"+(focused?" focus":""));
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const sel=inst&&inst.selectedIndex!=null?inst.selectedIndex:(spec.selectedIndex!=null?spec.selectedIndex:-1);
    // Host parity (first_visible_tree_index): once an INSTANCE exists its
    // expansion set is authoritative even when EMPTY; the spec's seed applies
    // only before any instance state. The scene skip-serializes an empty
    // expandedKeys, so "instance present, field absent" means all-collapsed —
    // falling back to the seed there made collapsing the last open folder a
    // visual no-op.
    const expanded=new Set(inst?(inst.expandedKeys||[]):(spec.expandedKeys||[]));
    const keys=spec.itemKeys||[];
    const open=[];
    (spec.nodes||[]).forEach((n,i)=>{
      const d=n.depth||0; open.length=d;
      const vis=open.every(Boolean);
      const key=keys[i]||"";
      const isOpen=n.hasChildren?(!!key&&expanded.has(key)):true;
      open.push(isOpen);
      if(!vis) return;
      const row=div("w-tree-row"+(i===sel?" sel":""));
      row.style.paddingLeft=(d*2)+"ch";
      // Right-click anywhere on the row (disclosure/checkbox included, like
      // the TUI's cell hit-test) fires `context` instead of the cell's own
      // event — the plugin raises a context menu anchored at the click cell.
      const treeCtx=e=>{ const c=cellAt(e); routeTree(ctx,spec,"context",{index:i,key,col:c.col,row:c.row}); };
      if(n.hasChildren){
        const g=document.createElement("span"); g.className="w-tree-disc"; g.textContent=isOpen?"▼ ":"▶ ";
        g.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,spec,"expand",{index:i,key,expanded:!isOpen}); };
        row.appendChild(g);
      }
      if(spec.checkable&&n.checked!=null){
        const cb=document.createElement("span"); cb.className="w-tree-check"; cb.textContent=n.checked?"☑ ":"☐ ";
        cb.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,spec,"toggle",{index:i,key,checked:!n.checked}); };
        row.appendChild(cb);
      }
      const t=document.createElement("span"); t.className="w-tree-text"; t.textContent=entryText(n.text||{});
      row.appendChild(t);
      // A click anywhere on a FOLDER row both selects it and toggles its
      // expansion — the disclosure glyph alone is a needle-thin target, and
      // "click a folder to fold it" is what every tree UI trains people to
      // expect. Leaf rows keep plain select.
      row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e);
        else { routeTree(ctx,spec,"select",{index:i,key});
               if(n.hasChildren) routeTree(ctx,spec,"expand",{index:i,key,expanded:!isOpen}); } };
      // Bordered card node (Tree.cardBorders + extraLines): wrap the
      // primary row and its continuation rows in a rounded box, the web
      // twin of the TUI's `╭─…─╮` card chrome. Non-card nodes (folder
      // headers) stay plain rows.
      const isCard=!!spec.cardBorders&&!n.hasChildren&&(n.extraLines||[]).length>0;
      const holder=isCard?div("w-tree-card"+(i===sel?" sel":"")):el;
      if(isCard){ holder.style.marginLeft=(d*2)+"ch"; row.style.paddingLeft="0"; }
      holder.appendChild(row);
      // Fixed-height card rows (Tree.itemHeight > 1): the node's
      // extraLines render as continuation rows below the primary line,
      // indented past the disclosure column. The whole card is one
      // click target and highlights as a block — same contract as the
      // TUI, which emits a select hit for every continuation row.
      for(const ex of (n.extraLines||[])){
        const xr=div("w-tree-xrow"+(i===sel?" sel":""));
        xr.style.paddingLeft=isCard?"2ch":((d*2)+2)+"ch";
        xr.textContent=entryText(ex);
        xr.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,spec,"select",{index:i,key}); };
        holder.appendChild(xr);
      }
      if(isCard) el.appendChild(holder);
    });
    return el;
  }
  if(kind==="overlay"){
    // The TUI floats the child over the following rows; here it renders as
    // a raised dropdown card in flow, right under the row that opened it.
    const el=div("w-overlay");
    if(spec.child) el.appendChild(widgetEl(spec.child, ctx));
    return el;
  }
  if(kind==="raw"&&spec.entries){ const el=div("w-raw"); el.textContent=spec.entries.map(entryText).join("\n"); return el; }
  if(kind==="windowEmbed"){
    // Embedding another editor window's live cells inside a native panel
    // needs its own scene region (the webui cell buffer carries pane
    // interiors only) — honest placeholder until that lands.
    const el=div("w-embed-ph");
    el.textContent="⧉ live preview is not available in the web UI yet";
    if(spec.rows) el.style.minHeight=(spec.rows*CH)+"px";
    return el;
  }
  const fb=div("w-unsupported"); fb.textContent="["+kind+"]"; return fb;
}

// Route a tree row interaction by identity (tree spec key + event + row
// payload). When the TUI recorded a hit for this row, reuse its exact
// payload + index so the bridge's strict identity tier matches; rows below
// the TUI's scroll window carry just the synthesized payload and resolve
// through the bridge's tree synthesis.
function routeTree(ctx, node, eventType, payload){
  const base={surface:"panel",plugin:ctx.plugin,panelId:ctx.panelId};
  const hit=(ctx.hits||[]).find(h=>h.widgetKind==="tree"&&h.eventType===eventType
    &&h.widgetKey===(node.key||"")&&h.payload&&h.payload.index===payload.index);
  const msg={...base,widgetKey:node.key||"",eventType,payload:hit?hit.payload:payload};
  if(hit) msg.hitIndex=hit.index;
  sendWidget(msg);
}

// Native Settings — a control's value rendered semantically (read from the
// shared model; keyboard drives edits via handle_key).
const SET_LIST_KINDS = new Set(["textList","map","objectArray","dualList"]);
// Inline control (toggle/number/dropdown/text/json/complex). `idx` = item index;
// `live` enables click routing (false inside the entry dialog, which is keyboard).
function settingControlEl(c, idx, live){
  const k=c.kind, el=document.createElement("span"); el.className="set-ctl set-"+k;
  if(k==="toggle"){ const sw=document.createElement("span"); sw.className="set-switch"+(c.checked?" on":""); el.appendChild(sw); if(live) sw.onmousedown=setHit("controlToggle",idx); }
  else if(k==="number"){
    const mk=(t,cls,hit)=>{const b=document.createElement("span");b.className=cls;b.textContent=t;if(live&&hit)b.onmousedown=setHit(hit,idx);return b;};
    el.appendChild(mk("−","set-step","controlDecrement")); const v=document.createElement("span"); v.className="set-num-v"; v.textContent=c.value; el.appendChild(v); el.appendChild(mk("+","set-step","controlIncrement"));
  }
  else if(k==="dropdown"){ const p=document.createElement("span"); p.className="set-pill"; p.textContent=(c.options[c.selected]||"—")+" ▾"; if(live) p.onmousedown=setHit("controlDropdown",idx); el.appendChild(p);
    if(c.open){ const d=div("set-dd"); c.options.forEach((o,i)=>{const r=div("set-dd-row"+(i===c.selected?" sel":""));r.textContent=o;if(live)r.onmousedown=setHit("controlDropdownOption",idx,i);d.appendChild(r);}); el.appendChild(d);} }
  else if(k==="text"){ const f=document.createElement("span"); f.className="set-field"; f.textContent=(c.value||c.placeholder||"")+(c.editing?"▌":""); if(live) f.onmousedown=setHit("controlText",idx); el.appendChild(f); }
  else if(k==="json"){ const f=document.createElement("span"); f.className="set-field mono"; f.textContent=(c.value||"").slice(0,80)||"{}"; if(live) f.onmousedown=setHit("controlText",idx); el.appendChild(f); }
  else if(k==="complex"){ el.textContent="‹"+c.typeName+"›"; el.classList.add("set-dim"); }
  return el;
}
// Full-width list block for composite controls (rows + an Add affordance).
function settingListEl(c, idx, live){
  const k=c.kind;
  // DualList has its own two-column layout (Available | Included) plus the
  // add/remove/move buttons; render it natively and route each click to the
  // matching SettingsHit (same dispatch the TUI uses).
  if(k==="dualList") return dualListEl(c, idx, live);
  const el=div("set-list"); let rows=[];
  // rowHit: (rowIndex) -> SettingsHit for clicking that row; addHit for "＋ Add".
  let rowHit=null, addHit=null;
  if(k==="textList"){ rows=c.items.map((t,i)=>({label:t, focused:i===c.focused})); rowHit=i=>["controlTextListRow",idx,i]; addHit=["controlTextListRow",idx,c.items.length]; }
  else if(k==="map"){ rows=c.entries.map((e,i)=>({label:e.key, sub:e.display, focused:i===c.focused})); rowHit=i=>["controlMapRow",idx,i]; if(!c.noAdd) addHit=["controlMapAddNew",idx]; }
  else if(k==="objectArray"){ rows=c.entries.map((t,i)=>({label:t, focused:i===c.focused})); rowHit=i=>["controlMapRow",idx,i]; }   // select item; Enter edits
  // Column header (`Name │ <column>`), mirroring the TUI's dimmed header row.
  if(k==="map"&&c.column&&rows.length){ const h=div("set-list-row set-list-head");
    const l=document.createElement("span"); l.className="set-list-label"; l.textContent="Name"; h.appendChild(l);
    const s=document.createElement("span"); s.className="set-list-sub"; s.textContent=c.column; h.appendChild(s);
    el.appendChild(h); }
  if(!rows.length){ const e=div("set-list-empty"); e.textContent="No entries"; el.appendChild(e); }
  rows.forEach((r,i)=>{ const row=div("set-list-row"+(r.focused?" sel":""));
    if(r.badge){ const b=document.createElement("span"); b.className="set-list-badge"; b.textContent=r.badge; row.appendChild(b); }
    const l=document.createElement("span"); l.className="set-list-label"; l.textContent=r.label; row.appendChild(l);
    if(r.sub){ const s=document.createElement("span"); s.className="set-list-sub"; s.textContent=r.sub; row.appendChild(s); }
    if(r.focused&&(k==="map"||k==="objectArray")){ const h=document.createElement("span"); h.className="set-list-hint"; h.textContent="[Enter to edit]"; row.appendChild(h); }
    if(live&&rowHit){ const h=rowHit(i); row.onmousedown=setHit(h[0],h[1],h[2]); }
    el.appendChild(row); });
  // Auto-managed maps (noAdd, e.g. Languages/LSP) hide the add row like the TUI.
  if(!(k==="map"&&c.noAdd)){ const add=div("set-list-add"+(c.addFocused?" sel":"")); add.textContent="＋ Add…"; if(live&&addHit) add.onmousedown=setHit(addHit[0],addHit[1],addHit[2]); el.appendChild(add); }
  return el;
}
// Two-column dual list (Available ⇄ Included) with native add/remove/move
// buttons. `idx` is the settings item index; row indices line up with the
// scene's `available`/`included` arrays (which mirror the control's own
// enumerations), so the dispatch hits select the right entry.
function dualListEl(c, idx, live){
  const el=div("set-dual");
  const col=(title, items, activeWhen, cursor, hitKind)=>{
    const cl=div("set-dual-col"+(c.activeColumn===activeWhen?" active":""));
    const h=div("set-dual-coltitle"); h.textContent=title; cl.appendChild(h);
    const list=div("set-dual-list");
    if(!items.length){ const e=div("set-list-empty"); e.textContent="—"; list.appendChild(e); }
    items.forEach((t,i)=>{ const r=div("set-dual-row"+(c.activeColumn===activeWhen&&i===cursor?" sel":""));
      r.textContent=t; if(live) r.onmousedown=setHit(hitKind,idx,i); list.appendChild(r); });
    cl.appendChild(list); return cl;
  };
  el.appendChild(col("Available", c.available, "available", c.availableCursor, "controlDualListAvailable"));
  // middle: add/remove + reorder buttons
  const mid=div("set-dual-mid");
  const mkb=(t,hit,title)=>{ const b=div("set-dual-btn"); b.textContent=t; b.title=title; if(live) b.onmousedown=setHit(hit,idx); return b; };
  mid.appendChild(mkb("→","controlDualListAdd","Add to included"));
  mid.appendChild(mkb("←","controlDualListRemove","Remove from included"));
  mid.appendChild(mkb("↑","controlDualListMoveUp","Move up"));
  mid.appendChild(mkb("↓","controlDualListMoveDown","Move down"));
  el.appendChild(mid);
  el.appendChild(col("Included", c.included, "included", c.includedCursor, "controlDualListIncluded"));
  return el;
}
// `entry` flag: rows belong to the add/edit sub-dialog, whose interaction is
// "select the item then keyboard-edit it". A row click routes to the entry
// dialog's own dispatch (kind "entryItem") and inner controls are display-only
// (live=false), matching the TUI's entry-dialog click behavior.
function settingItemRows(items, container, live, entry){
  for(const it of items){
    if(it.sectionStart && it.section){ const h=div("set-section"); h.textContent=it.section; container.appendChild(h); }
    const isList=SET_LIST_KINDS.has(it.control.kind);
    const row=div("set-item"+(it.selected?" sel":"")+(it.readOnly?" ro":"")+(isList?" set-item-block":""));
    const head=div("set-item-head");
    const nm=div("set-name"); nm.textContent=it.name+(it.modified?" •":""); head.appendChild(nm);
    const selectHit = entry
      ? (it.readOnly ? null : e=>{ e.preventDefault(); e.stopPropagation(); sendSettings({kind:"entryItem", a:it.index}); })
      : (live ? setHit("item",it.index) : null);  // click the row label = select item
    if(selectHit){ nm.onmousedown=selectHit; }
    if(!isList){ const ctl=div("set-ctl-wrap"); ctl.appendChild(settingControlEl(it.control,it.index,live&&!entry)); head.appendChild(ctl); if(entry&&selectHit) ctl.onmousedown=selectHit; }
    row.appendChild(head);
    if(it.description){ const d=div("set-desc"); d.textContent=it.description; row.appendChild(d); }
    if(isList) row.appendChild(settingListEl(it.control,it.index,live&&!entry));
    container.appendChild(row);
  }
}
function settingsEls(s){
  const out=[]; const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim);
  const el=div("region settings-modal");
  const title=div("set-title"); title.textContent=s.title+"  —  layer: "+s.targetLayer; el.appendChild(title);
  const body=div("set-body");
  // left: category tree
  const cats=div("set-cats"+(s.focus==="categories"?" focus":""));
  for(const c of s.categories){ const r=div("set-cat"+(c.selected?" sel":""));
    if(c.expandable){ const chev=document.createElement("span"); chev.className="set-cat-chev"; chev.textContent=c.expanded?"▼ ":"▶ "; chev.onmousedown=setHit("categoryDisclosure",c.index); r.appendChild(chev); } else r.appendChild(document.createTextNode("  "));
    const nm=document.createElement("span"); nm.textContent=c.name; r.appendChild(nm);
    r.onmousedown=setHit("category",c.index); cats.appendChild(r);
    if(c.expanded){ c.sections.forEach((sec,si)=>{ const sr=div("set-cat-sec"); sr.textContent="   "+sec; sr.onmousedown=setHit("categorySection",c.index,si); cats.appendChild(sr); }); } }
  body.appendChild(cats);
  // right: items, or search results
  const right=div("set-items"+(s.focus==="settings"?" focus":""));
  if(s.searchActive){ const sb=div("set-search"); sb.textContent="Search: "+s.searchQuery+"▌"; right.appendChild(sb);
    s.searchResults.forEach((r,i)=>{const rr=div("set-sresult"+(i===s.searchSelected?" sel":""));rr.textContent=r.name+"   — "+r.category;rr.onmousedown=setHit("searchResult",i);right.appendChild(rr);}); }
  else { settingItemRows(s.items, right, true); }
  body.appendChild(right);
  el.appendChild(body);
  // footer
  const ft=div("set-footer"); const fkinds=["layer","reset","save","cancel"];
  s.footerButtons.forEach((b,i)=>{
    const sel=i===s.footerSelected&&s.focus==="footer";
    const bb=div("btn"+(b==="Save"?" primary":"")+(sel?" sel":"")); bb.textContent=b; bb.onmousedown=setHit(fkinds[i]||"cancel"); ft.appendChild(bb);
  }); el.appendChild(ft);
  // entry dialog (Map/ObjectArray add/edit): rows are clickable (select+edit),
  // controls keyboard-driven, buttons routed by name to the entry dispatch.
  if(s.entryDialog){ const d=s.entryDialog; const dlg=div("set-overlay set-entry");
    const dt=div("set-entry-title"); dt.textContent=d.title; dlg.appendChild(dt);
    const di=div("set-entry-items"); settingItemRows(d.items.map((it,i)=>({...it,selected:i===d.selectedItem&&!d.focusOnButtons})), di, true, true); dlg.appendChild(di);
    const db=div("set-dlg-btns");
    // Button focus index follows the TUI order [Save, Cancel, Delete]; the
    // visual order here puts Delete in the middle but each routes by name.
    const tuiIdx={Save:0,Cancel:1,Delete:2};
    const btns=d.noDelete?["Save","Cancel"]:["Save","Delete","Cancel"];
    btns.forEach(b=>{const f=d.focusOnButtons&&tuiIdx[b]===d.focusedButton; const bb=div("btn"+(b==="Save"?" primary":b==="Delete"?" danger":"")+(f?" sel":"")); bb.textContent=b;
      bb.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendSettings({kind:"entryButton", button:b.toLowerCase()}); }; db.appendChild(bb);}); dlg.appendChild(db);
    el.appendChild(dlg); }
  if(s.showingHelp){ const h=div("set-overlay"); h.innerHTML="<b>Settings help</b><br>↑↓ navigate · Tab switch panel · Space/Enter activate · Ctrl+S save · Esc close"; el.appendChild(h); }
  out.push(el);
  return out;
}

// Native keybinding editor — full modal (header/search/filters, table, footer,
// help, add/edit dialog, confirm). Keyboard-driven: keystrokes are forwarded
// globally to handle_key, which the editor's own dispatch handles. The scrim
// swallows stray clicks so nothing mis-routes.
function keybindingEditorEls(kb){
  const out=[];
  const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim);
  const el=div("region kbedit");
  const title=div("kb-title"); title.textContent=kb.title; el.appendChild(title);

  // header: config path + search + filters
  const hdr=div("kb-header");
  const cfg=div("kb-cfg"); cfg.textContent="Config: "+kb.configPath+(kb.keymaps&&kb.keymaps.length?("   Maps: "+kb.keymaps.join(", ")):""); hdr.appendChild(cfg);
  if(kb.search.active){
    const sb=div("kb-search"+(kb.search.focused?" focus":""));
    const lbl=document.createElement("b"); lbl.textContent=kb.search.mode==="recordKey"?"REC KEY ":"SEARCH "; sb.appendChild(lbl);
    const val=document.createElement("span");
    val.textContent=kb.search.mode==="recordKey"?(kb.search.keyDisplay||"Press a key…"):(kb.search.query+(kb.search.focused?"▌":""));
    sb.appendChild(val); hdr.appendChild(sb);
  }
  const flt=div("kb-filters");
  const cf=document.createElement("span"); cf.className="kb-chip"+(kb.contextFiltered?" on":""); cf.textContent="Context: "+kb.contextFilter; flt.appendChild(cf);
  const sf=document.createElement("span"); sf.className="kb-chip"+(kb.sourceFiltered?" on":""); sf.textContent="Source: "+kb.sourceFilter; flt.appendChild(sf);
  const cnt=document.createElement("span"); cnt.className="kb-count"; cnt.textContent=kb.count+" bindings"+(kb.hasChanges?"  • modified":""); flt.appendChild(cnt);
  hdr.appendChild(flt);
  el.appendChild(hdr);

  // table
  const tbl=div("kb-table");
  const head=div("kb-row kb-head"); for(const c of ["Key","Action","Description","Context","Source"]){ const s=document.createElement("span"); s.className="kb-col kb-"+c.toLowerCase(); s.textContent=c; head.appendChild(s); } tbl.appendChild(head);
  const start=kb.scrollOffset||0, n=kb.viewport||kb.rows.length;
  const kbHit=i=>e=>{ e.preventDefault(); e.stopPropagation(); sendKbedit({a:i}); };
  for(let j=0;j<n;j++){ const idx=start+j, r=kb.rows[idx]; if(!r) break;
    if(r.type==="section"){ const row=div("kb-row kb-section"+(r.selected?" sel":"")); row.textContent=(r.collapsed?"▶ ":"▼ ")+r.name+" ("+r.count+")"; row.onmousedown=kbHit(idx); tbl.appendChild(row); continue; }
    const row=div("kb-row"+(r.selected?" sel":""));
    const cols=[["key",r.key],["action",r.action],["description",r.description],["context",r.context],["source",r.source]];
    for(const [cls,txt] of cols){ const s=document.createElement("span"); s.className="kb-col kb-"+cls; s.textContent=txt||""; row.appendChild(s); }
    row.onmousedown=kbHit(idx);
    tbl.appendChild(row);
  }
  el.appendChild(tbl);

  // footer (static hints)
  const ft=div("kb-footer");
  ft.textContent="Enter Edit · a Add · d Delete · / Search · r Record · c Context · s Source · ? Help · Ctrl+S Save · Esc Close";
  el.appendChild(ft);

  // help overlay
  if(kb.showingHelp){
    const h=div("kb-overlay kb-help");
    h.innerHTML="<b>Keybinding editor</b><br>↑↓ navigate · Enter edit · a add · d delete<br>/ search · r record-key · Tab toggle mode<br>c context filter · s source filter<br>Ctrl+S save · Esc close · ? toggle help";
    el.appendChild(h);
  }

  // edit / add dialog
  if(kb.editDialog){
    const d=kb.editDialog; const dlg=div("kb-overlay kb-dialog");
    const dt=div("kb-dlg-title"); dt.textContent=d.title; dlg.appendChild(dt);
    const fld=(label,val,focused,extra)=>{ const f=div("kb-field"+(focused?" focus":"")); const l=document.createElement("span"); l.className="kb-field-l"; l.textContent=label; f.appendChild(l);
      const v=document.createElement("span"); v.className="kb-field-v"; v.textContent=val; f.appendChild(v); if(extra){ const e2=document.createElement("span"); e2.className="kb-field-x"; e2.textContent=extra; f.appendChild(e2);} return f; };
    dlg.appendChild(fld("Key:", d.keyDisplay||(d.keyCapturing?"Press a key…":"(none)"), d.focusArea===0, d.keyCapturing?"(capturing)":""));
    const af=fld("Action:", d.actionText+(d.focusArea===1?"▌":""), d.focusArea===1, "");
    if(d.actionError){ const e2=document.createElement("span"); e2.className="kb-err"; e2.textContent="✗ "+d.actionError; af.appendChild(e2); }
    dlg.appendChild(af);
    if(d.autocomplete && d.autocomplete.length){ const ac=div("kb-autocomplete");
      d.autocomplete.forEach((s,i)=>{ const r=div("kb-ac-row"+(i===d.autocompleteSelected?" sel":"")); r.textContent=s; ac.appendChild(r); }); dlg.appendChild(ac); }
    dlg.appendChild(fld("Context:", "["+d.context+"]", d.focusArea===2, d.focusArea===2?"(← → cycle)":""));
    if(d.conflicts && d.conflicts.length){ const cf2=div("kb-conflicts"); cf2.innerHTML="<b>Conflicts:</b>"; for(const c of d.conflicts){ const r=document.createElement("div"); r.textContent="  "+c; cf2.appendChild(r);} dlg.appendChild(cf2); }
    const btns=div("kb-dlg-btns");
    const sb=div("kb-btn"+(d.saveFocused?" focus":"")); sb.textContent="[ Save ]"; btns.appendChild(sb);
    const cb=div("kb-btn"+(d.cancelFocused?" focus":"")); cb.textContent="[ Cancel ]"; btns.appendChild(cb);
    dlg.appendChild(btns);
    el.appendChild(dlg);
  }

  // confirm dialog
  if(kb.confirm){ const c=div("kb-overlay kb-confirm");
    const m=div("kb-confirm-msg"); m.textContent="Save changes before closing?"; c.appendChild(m);
    const b=div("kb-dlg-btns"); kb.confirm.buttons.forEach((label,i)=>{ const bb=div("kb-btn"+(i===kb.confirm.selected?" focus":"")); bb.textContent="[ "+label+" ]"; b.appendChild(bb); }); c.appendChild(b);
    el.appendChild(c);
  }

  out.push(el);
  return out;
}

// Native auxiliary modals (keybinding editor / event-debug / theme-info). A
// titled list of lines; anchored (theme-info) or centered. Interaction (nav /
// Esc / rebind) already flows through handle_key, so this is render-only.
function auxModalEls(m){
  const out=[];
  const anchored = m.rect && (m.rect.x || m.rect.y);
  if(!anchored){ const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim); }
  const el=div("region auxmodal "+(anchored?"anchored":"centered")+" am-"+m.kind);
  if(anchored){ el.style.left=px(m.rect.x,CW)+"px"; el.style.top=px(m.rect.y,CH)+"px"; }
  const title=div("am-title"); title.textContent=m.title; el.appendChild(title);
  const body=div("am-body");
  for(const ln of m.lines){ const r=div("am-line"+(ln.selected?" sel":"")); r.textContent=ln.text||" "; body.appendChild(r); }
  el.appendChild(body);
  if(m.footer){ const f=div("am-footer"); f.textContent=m.footer; el.appendChild(f); }
  out.push(el);
  return out;
}

// Native right-click / new-tab context menu. Rendered at the menu's cell
// position; clicking item i forwards a click at (x+1, y+1+i) — the cell the
// editor's hit-test resolves to that item (`item_idx = row - y - 1`).
function contextMenuEl(cm){
  const el=div("region ctxmenu");
  el.style.left=px(cm.x,CW)+"px"; el.style.top=px(cm.y,CH)+"px";
  cm.items.forEach((label,i)=>{
    const row=div("ctxitem"+(i===cm.highlighted?" sel":""));
    row.textContent=label;
    const cell={col:cm.x+1,row:cm.y+1+i};
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:cell.col,row:cell.row}); };
    row.onmouseenter=()=>sendMouse({kind:"moved",col:cell.col,row:cell.row});
    el.appendChild(row);
  });
  return el;
}

// A floating / dock plugin widget panel, rendered natively at its cell rect.
// Returns [scrim?, panel]: a `floatingModal` is a blocking dialog, so it gets a
// dimming `.modal-scrim` behind it (same pattern as the trust / settings /
// keybinding modals) that swallows mousedown so clicks can't reach the dock or
// buffer underneath. A `dock` is a persistent side panel and gets NO scrim.
function widgetSurfaceEls(s){
  const out=[];
  if(s.kind==="floatingModal"){
    // An ANCHORED panel (right-click context menu) keeps the scrim as a
    // click-catcher but undimmed — the TUI draws no background dim for it —
    // and a press outside the popup dismisses it (standard menu behaviour;
    // the centered modal instead swallows outside-clicks, it has explicit
    // Cancel / Esc).
    const scrim=div("modal-scrim"+(s.anchored?" scrim-clear":""));
    scrim.onmousedown=s.anchored
      ? e=>{ e.preventDefault(); e.stopPropagation(); sendKey({key:"Escape"}); }
      : e=>e.stopPropagation();
    out.push(scrim);
  }
  const el=div("region widget-surface w-"+s.kind+(s.anchored?" anchored":"")); place(el,s.rect);
  if(s.kind==="dock" && !isMobile() && s.rect.x===0){
    // COSMOS shell: the dock keeps its cell rect for hit-testing (widget
    // clicks forward LOGICAL cells, never pixel-derived ones), but its
    // visual card is inset from the device — a gap on the right where the
    // bezel's left rail lands — and stretched to the bezel's vertical
    // extents so it reads as its own floating glass panel.
    el.style.width=Math.max(140, px(s.rect.w,CW)-SHELL.side-SHELL.gap)+"px";
    el.style.top=(px(s.rect.y,CH)-SHELL.top+4)+"px";
    el.style.height=(px(s.rect.h,CH)+SHELL.top+SHELL.bot-8)+"px";
  }
  if(s.kind==="floatingModal"){
    // The host sizes the panel in whole terminal cells, but the DOM adds
    // per-row gaps + padding a cell grid can't express, so a snug dialog
    // (e.g. the New Folder dialog) would overflow its fixed height and clip
    // its buttons under `overflow:auto`. Treat the host height as a minimum
    // and let the modal grow to fit its content; nudge it up by half the
    // overflow so it stays visually centered. An anchored popup instead
    // stays pinned at its click cell — it grows downward, no recentering.
    const want=el.style.height; el.style.minHeight=want; el.style.height="auto";
    if(s.anchored){
      // Content-sized in both axes (host cells can't express DOM padding);
      // the host-clamped rect stays as the minimum so the popup never
      // shrinks below its TUI footprint.
      el.style.minWidth=el.style.width; el.style.width="auto"; el.style.maxWidth="60vw";
    } else requestAnimationFrame(()=>{
      const grew=el.offsetHeight-parseFloat(want||"0");
      if(grew>0) el.style.top=(parseFloat(el.style.top)-grew/2)+"px";
    });
  }
  const ctx={ kind:"panel", plugin:s.plugin, panelId:s.panelId, hits:s.hits, instances:s.instances, focusKey:s.focusKey };
  el.appendChild(widgetEl(s.spec, ctx));
  // A left dock's rightmost column is an editor resize border (dock_resizing);
  // give it an explicit grip since the .widget-surface is in onChrome.
  if(s.kind==="dock") el.appendChild(borderDragHandle(s.rect.x + s.rect.w - 1, s.rect.y, s.rect.h));
  out.push(el);
  return out;
}

