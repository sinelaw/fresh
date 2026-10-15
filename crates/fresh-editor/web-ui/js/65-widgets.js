// Plugin widget tree (WidgetSpec) + Settings / keybinding editor / aux modals.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native plugin widgets (WidgetSpec tree) ----------------------------
// Renders the serialized WidgetSpec natively (Row/Col→flex, Toggle/Button→
// native controls, …). Toggle/Button clicks route back to the owning plugin
// via the same event the TUI fires; `surface` selects the routing path.
// Concatenate a TextPropertyEntry's display text (segments preferred).
/** function entryText(TextPropertyEntry) => String */
function entryText(e){ const sg=e.segments; return (sg!=null&&sg.length>0)?sg.map(s=>s.text||"").join(""):(e.text||""); }
// A style's look, as the host resolved its colours (`fgCss`/`bgCss`, from the
// theme keys a plugin names) — applied inline to the span it covers.
/** function applyStyle(El, OverlayOptions | Undefined) => Undefined */
function applyStyle(el, st){
  if(!st) return;
  if(st.fgCss) el.style.color=st.fgCss;
  if(st.bgCss){ el.style.background=st.bgCss; el.classList.add("w-chip"); }
  if(st.bold) el.style.fontWeight="600";
  if(st.italic) el.style.fontStyle="italic";
  if(st.underline) el.style.textDecoration="underline";
}
// A TextPropertyEntry as styled spans: its segments, each in its own colour,
// inside the entry's own style.
/** function fillEntry(El, TextPropertyEntry) => El */
function fillEntry(el, e){
  if(!e) return el;
  applyStyle(el, e.style);
  const segs=e.segments;
  if(segs!=null&&segs.length>0){
    for(const sg of segs){ const sp=document.createElement("span"); sp.textContent=sg.text||""; applyStyle(sp, sg.style); el.appendChild(sp); }
  } else el.append(e.text||"");
  return el;
}

// A fixed-width form-label cell ("Agent:") so a column of controls aligns.
// The width (in ch) is the spec's `labelWidth` — the display-column budget the
// TUI pads the label to — carried through instead of discarded, so the web can
// build the same aligned grid. The stylesheet owns the actual look/alignment
// per theme; trailing pad spaces are trimmed since the width is what aligns.
// A control's label with the one colon a form label takes — a plugin label
// that already ends in its own colon keeps it, rather than reading "Search::".
/** function labelColon(String) => String */
function labelColon(text){
  const t=(text||"").replace(/\s+$/,"");
  return /[:：]$/.test(t)?t+" ":t+": ";
}
// Accessibility: each control says what it is (role), what it is called
// (name) and where it stands (state), so assistive tech — and tests, by role
// and name — find it as the control it draws. Nothing takes the keyboard
// (the editor owns every key), so there is no tabindex here.
/** type AriaValue = String | Number | Boolean | Null */
/** function aria(El, String, Dict<AriaValue>) => El */
function aria(el, role, attrs){
  if(role) el.setAttribute("role", role);
  for(const [k, v] of Object.entries(attrs)){
    if(v==null||v===false&&k!=="checked"&&k!=="selected"&&k!=="expanded") continue;
    el.setAttribute("aria-"+k, String(v));
  }
  return el;
}
/** function formLabel(String, Number) => El */
function formLabel(text, labelWidth){
  const l=document.createElement("span");
  l.className="w-flabel";
  l.textContent=labelColon(text);
  if(labelWidth>0) l.style.setProperty("--flw", String(labelWidth));
  return l;
}

// Route a widget interaction. `ctx.kind` is "toolbar" (the prompt toolbar,
// routed by key) or "panel" (a natively laid-out plugin panel, see
// 73-native.js). A panel press is the control's IDENTITY — widget key, the
// event its kind answers a press with, the row — and the bridge rebuilds the
// event the terminal's tree would have built for the same control from the
// mounted spec (app/native_panels.rs). So nothing here decides what a press
// means, and a list row the terminal scrolled out of its window resolves
// like any other.
// The context widgetEl's callers pass (50-palette.js's toolbar, 73-native.js's
// panels), and the full one the renderer threads through the tree (`grid`:
// inside a grid row; `table`: inside a table; `instances` null = none).
/** type WidgetCtx =
    { kind: "toolbar", focusKey: String | Undefined }
    | { kind: "panel", slot: String, section: Number | Undefined, pane: Number | Undefined,
        focusKey: String, instances: Dict<WidgetInstance>, native: Boolean } */
/** type WCtx = {
    kind: "toolbar" | "panel", slot: String | Undefined, section: Number | Undefined,
    pane: Number | Undefined, focusKey: String | Undefined,
    instances: Dict<WidgetInstance> | Null, native: Boolean, grid: Boolean, table: Boolean,
    inRow: Boolean } */
// An undefined section/pane is left out of the JSON, as before.
/** function panelBase(WCtx) => {surface: String, slot: String | Undefined, section: Number | Undefined, pane: Number | Undefined} */
function panelBase(ctx){
  return {surface:"panel",slot:ctx.slot,section:ctx.section,pane:ctx.pane};
}
// A press on a toggle/button/text widget (`kind`) keyed `key`.
/** function routeWidget(WCtx, String | Undefined, String) => Undefined */
function routeWidget(ctx, key, kind){
  if(ctx.kind==="toolbar"){ if(key) sendWidget({surface:"toolbar",key}); return; }
  const base=panelBase(ctx);
  if(key){
    const et=kind==="button"?"activate":kind==="toggle"?"toggle":kind==="text"?"focus":null;
    if(et) sendWidget({...base,widgetKey:key,eventType:et,payload:{}});
  }
}
// A press on row `listIndex` of the list keyed `key`.
/** function routeListItem(WCtx, String | Undefined, String[], Number, UiEvent) => Undefined */
function routeListItem(ctx, key, itemKeys, listIndex, ev){
  if(ctx.kind==="toolbar"){ if(key) sendWidget({surface:"toolbar",key}); return; }
  const base=panelBase(ctx);
  const itemKey=itemKeys[listIndex]??"";
  const payload={index:listIndex,key:itemKey,list_key:key||null};
  // Right-click raises the plugin's context menu, anchored at the cell.
  if(ev.button===2){
    const c=cellAt(ev);
    sendWidget({...base,widgetKey:itemKey,eventType:"context",payload,col:c.col,row:c.row});
    return;
  }
  sendWidget({...base,widgetKey:itemKey,eventType:"select",payload,clicks:ev.detail||1});
}

// A control press by key + the specific event (a dropdown answers both
// `dropdown_toggle` and `dropdown_select`).
/** function routeControl<T>(WCtx, String, String, T) => Undefined */
function routeControl(ctx,key,eventType,payload){
  if(ctx.kind!=="panel") return;
  sendWidget({...panelBase(ctx),widgetKey:key,eventType,payload});
}

// UTF-8 byte offset ⇄ UTF-16 code-unit index conversions for text widgets:
// the host TextEdit speaks bytes, input.selectionStart speaks UTF-16.
/** function byteToUtf16(String, Number) => Number */
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
/** function utf16ToByte(String, Number) => Number */
function utf16ToByte(s,idx){
  let b=0;
  for(let i=0;i<Math.min(idx,s.length);i++){
    const c=s.codePointAt(i);
    b+=c<0x80?1:c<0x800?2:c<0x10000?3:4;
    if(c>0xffff) i++;
  }
  return b;
}

// A single body-level host for dropdown option lists. Portaling the list OUT
// of its modal is what lets it never be clipped: a floating modal / the
// Settings dialog is a `transform`ed, `overflow`-clipping box, so ANY
// descendant (even `position:fixed`) is clipped to it. As a child of <body>
// the list has the viewport as its containing block and nothing clips it — the
// approach every portal-based design system (Radix, MUI, …) uses. The host is
// 0×0 (its children are all fixed-positioned), so it never intercepts clicks.
/** function popoverHost() => El */
function popoverHost(){
  const h=document.getElementById("fresh-popover-host");
  if(h) return h;
  const n=document.createElement("div"); n.id="fresh-popover-host"; document.body.appendChild(n);
  return n;
}
// Mount a dropdown option list into the body-level host, tagged with the region
// currently rendering (renderRegion sets `popoverRegionOwner`). That region
// reaps its own tagged pop-overs on its next fill, so the list disappears the
// moment the dropdown closes and can never leak/accumulate across frames.
/** function mountPopover(El) => Undefined */
function mountPopover(el){
  el.dataset.popoverRegion=popoverRegionOwner??"";
  popoverHost().appendChild(el);
}

// Position a `position:fixed` popover (dropdown option list) under its trigger
// so it is never clipped by an ancestor's scroll/overflow — a fixed box is only
// clipped by its containing block, not by intervening `overflow` scrollers like
// the Settings list or a plugin panel body. Mirrors what native macOS pop-up
// menus and Windows combo dropdowns do when items are wider than the closed
// control: the list is AT LEAST the trigger width and grows to fit its widest
// item, opens under the trigger, and SHIFTS/FLIPS to stay on screen rather than
// truncating. (Apple HIG: "the width of a pop-up menu should be wide enough to
// accommodate the longest item"; WinUI: dropdown min-width = control width,
// grows with content — neither forces the closed control to the widest item.)
//   - min-width = trigger width (never narrower than the trigger);
//   - left-align the list under the trigger; if that would spill past the
//     right edge, right-align it (grow leftward) — the shift a right-flush
//     control needs (e.g. the Settings controls, which sit at the dialog edge);
//   - flip above when there is no room below;
//   - clamp to the viewport so it's never pushed off-screen.
// Callers portal the list to the body-level host (see mountPopover), so the
// fixed containing block is the viewport; the ancestor walk below is a
// defensive fallback for any future in-tree caller.
/** function positionFloatingPopover(El, El, {align: String}) => Undefined */
function positionFloatingPopover(anchor, popup, opts){
  if(!anchor||!anchor.isConnected||!popup) return;
  // Fixed-positioning containing block: the viewport, unless an ancestor
  // establishes one (transform / filter / backdrop-filter / perspective /
  // will-change). Portaled popovers live under <body> so this is normally the
  // viewport; the walk stays for robustness (and for any non-portaled caller).
  let cb={left:0,top:0,right:window.innerWidth,bottom:window.innerHeight};
  for(let up=popup.parentElement; up; ){
    const el=up; if(!el) break;   // (`up` is reassigned, so it isn't narrowed)
    const cs=getComputedStyle(el), v=p=>cs.getPropertyValue(p);
    const set=p=>{ const x=v(p); return x!==""&&x!=="none"; };
    if(v("transform")!=="none"||v("perspective")!=="none"
       ||set("filter")||set("backdrop-filter")||set("-webkit-backdrop-filter")
       ||/transform|filter|perspective/.test(v("will-change"))){
      const r=el.getBoundingClientRect();
      cb={left:r.left,top:r.top,right:r.right,bottom:r.bottom}; break;
    }
    up=el.parentElement;
  }
  const a=anchor.getBoundingClientRect();
  popup.style.minWidth=`${a.width}px`;       // >= trigger, grow-to-content past it
  const w=popup.offsetWidth, h=popup.offsetHeight, M=4;
  // Horizontal alignment to the trigger. `align:"end"` aligns the right edges
  // (list grows leftward) — the right thing for a control flush against a
  // container's right edge (the Settings controls), so the wider list opens
  // INTO the dialog rather than spilling past it. Default `"start"` aligns the
  // left edges (grows rightward) for controls with room to their right (the
  // plugin form dropdowns). Either way, clamp to the viewport so it's never
  // pushed off-screen.
  let left = (opts.align==="end") ? (a.right-w) : a.left;
  left=Math.max(cb.left+M, Math.min(left, cb.right-w-M));
  // Vertical: prefer below the trigger; flip above when there is no room.
  let top=a.bottom+2;
  if(top+h > cb.bottom-M && a.top-h-2 >= cb.top+M) top=a.top-h-2;
  top=Math.max(cb.top+M, Math.min(top, cb.bottom-h-M));
  popup.style.left=`${left-cb.left}px`;
  popup.style.top=`${top-cb.top}px`;
}

// After a widgets-region rebuild: hand real DOM focus to the host-focused
// text widget's input (so the native caret blinks and IME composes there),
// pin its caret to the host TextEdit's cursor, and fall back to the hidden
// sink when no widget input holds focus anymore. Skipped mid-IME-composition
// so a caret write can't cancel the preedit.
/** function syncWidgetInputFocus() => Undefined */
function syncWidgetInputFocus(){
  const t=document.querySelector(".w-text-input[data-wfocus='1']");
  if(t){
    if(document.activeElement!==t) t.focus({preventScroll:true});
    const caret=t.dataset.caret;
    if(!t.dataset.composing&&caret!==""&&caret!=null){
      const i=Number(caret);
      try{ t.setSelectionRange(i,i); }catch(_){}
    }
  } else {
    const ae=document.activeElement;
    if(ae&&ae.classList.contains("w-text-input")) focusSink();
  }
}

/** function widgetEl(WidgetSpec, WidgetCtx) => El */
function widgetEl(spec, ctx){
  return widgetNode(spec, ctx.kind==="panel"
    ? {kind:"panel", slot:ctx.slot, section:ctx.section, pane:ctx.pane, focusKey:ctx.focusKey,
       instances:ctx.instances, native:ctx.native, grid:false, table:false, inRow:false}
    : {kind:"toolbar", slot:undefined, section:undefined, pane:undefined, focusKey:ctx.focusKey,
       instances:null, native:false, grid:false, table:false, inRow:false});
}
// The host instance state of the widget keyed `key`, if any.
/** function instOf(WCtx, String | Undefined) => WidgetInstance | Null */
function instOf(ctx, key){
  const m=ctx.instances;
  return (m!==null&&key!==undefined&&key!=="")?(m[key]??null):null;
}
// A widget's key (a label has none).
/** function specKey(WidgetSpec) => String | Undefined */
function specKey(s){ return s.kind==="label"?undefined:s.key; }
// A grid row's cells (none for anything else).
/** function rowCells(WidgetSpec) => WidgetSpec[] */
function rowCells(s){ return s.kind==="row"?s.children:[]; }
/** function widgetNode(WidgetSpec, WCtx) => El */
function widgetNode(spec, ctx){
  if(!spec) return document.createTextNode("");
  const kind=spec.kind, fk=ctx.focusKey, wkey=specKey(spec);
  const focused = !!wkey && wkey===fk;
  const kids=spec.kind==="row"?spec.children:spec.kind==="col"?spec.children:null;
  if(kids){
    // A row that lays text out in cells (`raw` cells between its controls,
    // labels padded with spaces to a column) only lines up in the buffer's
    // font, so it is drawn in it: a GRID row, whose controls still answer as
    // controls. Every other row is laid out by the browser.
    // A `raw` cell with text in it is cell-drawn content; one of only spaces
    // is a gap, and does not make its row a grid.
    /** const inked: (WidgetSpec) => Boolean */
    const inked=x=>x.kind==="raw"?x.entries.some(e=>entryText(e).trim()!==""):false;
    const isGridRow=c=>rowCells(c).some(inked);
    // A column of grid rows that all have the same number of cells is a
    // TABLE the terminal drew line by line (a menu's two columns of entries
    // with a rule between them, Tab walking it in reading order). Natively
    // it is one CSS grid: the columns line up in any font, the cells are
    // laid out as text and controls, and a `│` cell becomes a hairline.
    if(ctx.native&&!ctx.grid&&kind==="col"&&kids.length>1&&kids.every(isGridRow)
       &&kids.every(c=>rowCells(c).length===rowCells(kids[0]).length)){
      const el=div("w-col w-table");
      el.style.gridTemplateColumns=`repeat(${rowCells(kids[0]).length},auto)`;
      const tctx={...ctx,table:true};
      for(const r of kids){
        const re=div("w-row w-table-row");
        for(const c of rowCells(r)) re.appendChild(widgetNode(c, tctx));
        el.appendChild(re);
      }
      return el;
    }
    const grid=!ctx.table&&(ctx.grid||(kind==="row"&&kids.some(inked)));
    const wrap=spec.kind==="row"&&spec.wrap, end=spec.kind==="row"&&spec.justifyEnd;
    const el=div("w-"+kind+(wrap?" wrap":"")+(grid&&!ctx.grid?" grid":"")+(end?" end":""));
    const cctx={...ctx,grid:grid||ctx.grid,inRow:kind==="row"};
    for(const c of kids) el.appendChild(widgetNode(c, cctx));
    return el;
  }
  if(spec.kind==="label"){
    // A terminal section heading: a caption followed by a rule of box-drawing
    // characters long enough to reach any edge. Natively, the caption and a
    // drawn rule that ends where the panel does.
    const segs=spec.segments, hasSegs=segs!=null&&segs.length>0;
    const heading=(!ctx.grid&&!hasSegs)
      ?/^\s*[─━═]*\s*([^─━═]*?)\s*[─━═]{3,}\s*$/.exec(spec.text||""):null;
    if(heading){
      const el=div("w-heading");
      const t=document.createElement("span"); t.textContent=heading[1]??""; el.appendChild(t);
      el.appendChild(div("w-heading-rule"));
      return el;
    }
    const el=div("w-label-row");
    applyStyle(el, spec.style);
    const seg=(text,st)=>{ const sp=document.createElement("span"); sp.textContent=text; applyStyle(sp, st); el.appendChild(sp); };
    if(hasSegs) for(const sg of segs) seg(sg.text||"", sg.style);
    else seg(spec.text||"", undefined);
    if(spec.labelWidth>0) el.style.paddingLeft=`${spec.labelWidth}ch`;
    if(spec.wrap) el.classList.add("wrap");
    return el;
  }
  if(spec.kind==="toggle"){
    const el=div("w-toggle"+(spec.checked?" on":"")+(focused?" focus":"")+(spec.labelFirst?" labelfirst":""));
    // A sliding switch (CSS-drawn), matching the Settings UI toggles —
    // not a unicode ☑/☐ glyph. `labelFirst` renders the form layout
    // (`Auto mode : [switch]`) so the chip aligns under a column of controls;
    // the default keeps the chip-first `[switch] label` layout.
    const box=document.createElement("span"); box.className="w-box";
    if(spec.labelFirst){
      if(spec.label) el.appendChild(formLabel(spec.label, spec.labelWidth));
      el.appendChild(box);
    } else {
      el.appendChild(box);
      const lb=document.createElement("span"); lb.className="w-label"; lb.textContent=spec.label||""; el.appendChild(lb);
    }
    aria(el, "switch", {checked:!!spec.checked, label:(spec.label||"").replace(/\s*:\s*$/,"").trim()||null});
    if(wkey) el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,wkey,kind); };
    return el;
  }
  if(spec.kind==="button"){
    const el=div("w-button "+(spec.intent||"normal")+(spec.disabled?" disabled":"")+(focused?" focus":"")
      +(spec.bare?" bare":"")+(spec.fullWidth?" full":""));
    // A terminal label carries its own padding (a bare title bar's trailing
    // spaces fill the row to the close box); a laid-out button has a box. In
    // a grid row the padding IS the layout, so it stays.
    const label=ctx.grid?(spec.label||""):(spec.label||"").trim();
    // A menu entry that says what it sets: `(•)`/`( )` a choice, `[✓]`/`[ ]`
    // a switch — drawn as the marks they stand for.
    const mark=(!ctx.grid&&ctx.native)?/^(\(•\)|\( \)|\[✓\]|\[ \])\s+(.*)$/.exec(label):null;
    if(mark){
      const mtxt=mark[1]??"", radio=mtxt[0]==="(", on=mtxt[1]!==" ";
      const m=document.createElement("span");
      m.className=(radio?"w-mark-radio":"w-mark-check")+(on?" on":"");
      el.appendChild(m); el.append(mark[2]??"");
    } else el.textContent=label;
    if(ctx.grid) el.classList.add("gridcell");
    if(mark) aria(el, (mark[1]??"")[0]==="("?"menuitemradio":"menuitemcheckbox", {checked:(mark[1]??"")[1]!==" ", label:(mark[2]??"").trim()});
    else aria(el, "button", {disabled:!!spec.disabled, label:label.trim()!==label?label.trim():null});
    if(wkey && !spec.disabled) el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,wkey,kind); };
    return el;
  }
  if(spec.kind==="spacer"){ const el=div("w-spacer"); if(spec.flex) el.style.flex="1"; else el.style.width=`${(spec.cols||1)*CW}px`; return el; }
  if(kind==="divider"){ return div("w-divider"); }
  if(spec.kind==="hintBar"){
    const el=div("w-hintbar");
    for(const h of (spec.entries||[])){ const e2=document.createElement("span"); e2.className="w-hint";
      e2.innerHTML='<b>'+esc(h.keys||"")+'</b> '+esc(h.label||""); el.appendChild(e2); }
    return el;
  }
  if(spec.kind==="labeledSection"){
    const el=div("w-section"); if(spec.label){ const l=div("w-section-label"); l.textContent=spec.label; el.appendChild(l); }
    aria(el, "group", {label:spec.label||null});
    if(spec.child) el.appendChild(widgetNode(spec.child, ctx)); return el;
  }
  if(spec.kind==="text"){
    // A REAL <input>/<textarea>, but the editor stays the single source of
    // truth: the host TextEdit's live value + caret arrive via instance
    // state (instances[key].textValue / cursorByte) and are written into
    // the element on every frame. Keystrokes never edit the element
    // locally — the global keydown handler preventDefaults and forwards
    // them, and the pushed frame echoes the authoritative result back.
    // The native element buys the real caret, focus ring, IME target and
    // browser-computed click-to-caret positions for free.
    const inst=instOf(ctx, wkey);
    const val=(inst&&inst.textValue!=null)?inst.textValue:(spec.value||"");
    // Width as the terminal sizes it: a multi-line field (or one asked to)
    // fills its row; a `fieldWidth` is a width in characters.
    const fill=spec.fullWidth||((spec.rows||1)>1&&!spec.fieldWidth);
    const el=div("w-text"+(focused?" focus":"")+(fill?" fill":""));
    if(!fill&&spec.fieldWidth>0) el.style.width=`calc(${spec.fieldWidth}ch + 1.2em)`;
    // A form-style field (`label` + `labelWidth`) keeps its label OUTSIDE the
    // bordered field, in the shared label column, so the label doesn't get
    // boxed into the input and the field aligns with the other controls. A
    // plain labelled field (no width) keeps the legacy inline label.
    const formField=!!spec.label&&spec.labelWidth>0;
    if(spec.label&&!formField){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=labelColon(spec.label); el.appendChild(l); }
    const multi=(spec.rows||1)>1;
    const input=document.createElement(multi?"textarea":"input");
    input.className="w-text-input";
    if(multi) input.setAttribute("rows", String(spec.rows));
    input.value=val;
    input.placeholder=spec.placeholder||"";
    aria(input, "", {label:(spec.label?labelColon(spec.label).replace(/\s*:\s*$/,""):spec.placeholder)||null,
      multiline:multi||null});
    input.setAttribute("autocapitalize","off"); input.setAttribute("autocomplete","off");
    input.setAttribute("autocorrect","off"); input.setAttribute("spellcheck","false");
    // Block every local edit outside the key-forwarding path (autofill,
    // drag-drop text, context-menu paste): the paste event still bubbles to
    // the document paste listener (the one real paste path), and IME
    // composition — which beforeinput can't cancel — is forwarded on commit
    // exactly like the hidden sink does, then discarded locally.
    input.addEventListener("beforeinput",ev=>{ if(!ev.isComposing) ev.preventDefault(); });
    input.addEventListener("compositionstart",()=>{ input.dataset.composing="1"; });
    input.addEventListener("compositionend",ev=>{ input.removeAttribute("data-composing"); const d=ev.data; if(d) sendPaste(d); input.value=val; });
    input.addEventListener("input",ev=>{ if(!ev.isComposing) input.value=val; });
    // Mousedown focuses the widget host-side (the same `focus` event a TUI
    // click fires). No preventDefault — the browser places its caret
    // natively, and mouseup reports that position so the host TextEdit
    // follows. (A drag-selection is left as a browser-local convenience
    // for copy; the host tracks selection only via its own keybindings.)
    input.addEventListener("mousedown",e=>{ e.stopPropagation(); if(wkey) routeWidget(ctx,wkey,kind); });
    input.addEventListener("mouseup",e=>{ e.stopPropagation();
      if(wkey && input.selectionStart===input.selectionEnd)
        sendWidget({...panelBase(ctx),widgetKey:wkey,eventType:"focus",
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
    const comps=inst?inst.completions:undefined;
    if(focused&&inst&&comps!=null&&comps.length>0){
      const dd=div("w-complete");
      const nav=!!inst.completionNavigated, csel=inst.completionSelected||0;
      comps.forEach((cand,i)=>{
        const r=div("w-complete-row"+((nav&&i===csel)?" sel":""));
        r.textContent=cand; dd.appendChild(r);
      });
      el.appendChild(dd);
    }
    if(formField){
      const rowEl=div("w-field-row");
      rowEl.appendChild(formLabel(spec.label, spec.labelWidth));
      rowEl.appendChild(el);
      return rowEl;
    }
    return el;
  }
  if(spec.kind==="radio"){
    const inst=instOf(ctx, wkey);
    const sel=(inst&&inst.selectedIndex!=null)?inst.selectedIndex:(spec.selectedIndex||0);
    const el=aria(div("w-radio"+(focused?" focus":"")), "radiogroup", {label:spec.label||null});
    if(spec.label) el.appendChild(formLabel(spec.label, spec.labelWidth));
    else if(spec.labelWidth>0) el.style.paddingLeft=`${spec.labelWidth}ch`;
    (spec.options||[]).forEach((o,i)=>{
      const opt=aria(div("w-radio-opt"+(i===sel?" on":"")), "radio", {checked:i===sel, label:o});
      opt.appendChild(div("w-radio-dot"));
      const t=document.createElement("span"); t.textContent=o; opt.appendChild(t);
      opt.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(wkey) routeControl(ctx,wkey,"radio_select",{index:i}); };
      el.appendChild(opt);
    });
    return el;
  }
  if(spec.kind==="number"){
    const inst=instOf(ctx, wkey);
    const num=(inst&&inst.numberValue!=null)?inst.numberValue:(spec.value||0);
    const el=div("w-number"+(focused?" focus":""));
    if(spec.label){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=labelColon(spec.label); el.appendChild(l); }
    const disp=spec.percent?`${Math.round(num*100)}%`
      :spec.integer?String(Math.trunc(num))
      :String(Number(toFixed(num, 3)));
    // −/+ steppers focus the widget then step it via the host's own
    // Left/Right handling — the value math (min/max/step) stays host-side.
    const mk=(t,key)=>{ const b=document.createElement("span"); b.className="w-num-step"; b.textContent=t;
      b.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(!wkey) return;
        routeControl(ctx,wkey,"number_value",{}); sendKey({key}); };
      return b; };
    el.appendChild(mk("−","ArrowLeft"));
    const v=document.createElement("span"); v.className="w-num-val"; v.textContent=disp;
    aria(v, "spinbutton", {valuenow:num, valuetext:disp, label:spec.label||null});
    v.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(wkey) routeControl(ctx,wkey,"number_value",{}); };
    el.appendChild(v);
    el.appendChild(mk("+","ArrowRight"));
    return el;
  }
  if(spec.kind==="dropdown"){
    const inst=instOf(ctx, wkey);
    const selIdx=(inst&&inst.selectedIndex!=null)?inst.selectedIndex:(spec.selectedIndex||0);
    const open=inst?!!inst.dropdownOpen:!!spec.open;
    const el=div("w-dropdown"+(focused?" focus":"")+(!!spec.label&&spec.labelWidth>0?" wform":""));
    if(spec.label){
      if(spec.labelWidth>0) el.appendChild(formLabel(spec.label, spec.labelWidth));
      else { const l=document.createElement("span"); l.className="w-text-label"; l.textContent=labelColon(spec.label); el.appendChild(l); }
    }
    const pill=document.createElement("span"); pill.className="w-dd-pill";
    aria(pill, "combobox", {haspopup:"listbox", expanded:open, label:spec.label||null});
    // Small up/down chevrons — a MATCHED-SIZE pair (▴ U+25B4 / ▾ U+25BE). The
    // full-size ▲ (U+25B2) is ~2× the width of the small ▾ in a proportional
    // font (e.g. the macOS skin's -apple-system), so pairing it with ▾ made the
    // pill jump ~6px wider on open; the small ▴ keeps the width stable.
    pill.textContent=((spec.options||[])[selIdx]??"—")+(open?" ▴":" ▾");
    pill.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(wkey) routeControl(ctx,wkey,"dropdown_toggle",{}); };
    el.appendChild(pill);
    if(open){
      const dd=aria(div("w-dd w-dd-floating"), "listbox", {label:spec.label||null});
      (spec.options||[]).forEach((o,i)=>{
        const r=aria(div("w-dd-row"+(i===selIdx?" sel":"")), "option", {selected:i===selIdx}); r.textContent=o;
        r.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(wkey) routeControl(ctx,wkey,"dropdown_select",{index:i}); };
        dd.appendChild(r);
      });
      // Portal the option list to the body-level host so it extends PAST the
      // modal's border and is never clipped by the surface's overflow (parity
      // with the TUI popover). Shared positioner anchors it under the pill,
      // grows it to fit the widest item, and flips/shifts to stay on screen.
      mountPopover(dd);
      requestAnimationFrame(()=>positionFloatingPopover(pill, dd, {align:"start"}));
    }
    return el;
  }
  if(spec.kind==="dualList"){
    // Two-column ordered-subset picker. Rows are derived exactly like the
    // host: Available = options minus included minus excluded (in option
    // order); Included = the host-owned ordered instance list. Clicks fire
    // the same `dual_focus` hits a TUI cell click resolves; moves/reorders
    // stay keyboard-driven through the host (Space / PageUp / PageDown).
    const inst=instOf(ctx, wkey);
    // `included` is skip-serialized when empty, so detect "host state
    // exists" via activeIncluded (always present for a DualList instance).
    const included=(inst&&inst.activeIncluded!=null)?(inst.included||[]):(spec.included||[]);
    const activeIncluded=inst?!!inst.activeIncluded:false;
    // (DualList options are always {value, label} objects.)
    const optLabel=v=>{ const o=(spec.options||[]).find(o=>o.value===v); return o?o.label:v; };
    const excluded=new Set([...(spec.excluded||[]),...included]);
    const avail=(spec.options||[]).map(o=>o.value).filter(v=>!excluded.has(v));
    const el=div("w-dual"+(focused?" focus":""));
    if(spec.label){ const l=div("w-section-label"); l.textContent=spec.label; el.appendChild(l); }
    const cols=div("w-dual-cols");
    const mkCol=(title,values,column,active,cursor)=>{
      const c=div("w-dual-col"+(active?" active":""));
      const t=div("w-dual-title"); t.textContent=title; c.appendChild(t);
      values.forEach((v,i)=>{
        const r=div("w-dual-row"+(active&&i===cursor?" sel":"")); r.textContent=optLabel(v);
        r.onmousedown=e=>{ e.preventDefault(); e.stopPropagation();
          if(wkey) routeControl(ctx,wkey,"dual_focus",{column,index:i}); };
        c.appendChild(r);
      });
      return c;
    };
    cols.appendChild(mkCol("Available",avail,"available",!activeIncluded,inst?(inst.availableCursor||0):0));
    cols.appendChild(mkCol("Included",included,"included",activeIncluded,inst?(inst.includedCursor||0):0));
    el.appendChild(cols);
    return el;
  }
  if(spec.kind==="list"){
    const el=aria(div("w-list"), "listbox", {});
    const inst=instOf(ctx, wkey);
    const sel=inst?inst.selectedIndex:(spec.selectedIndex!=null?spec.selectedIndex:-1);
    const specs=spec.itemSpecs||[];
    if(specs.length){
      // Card list: each logical item is a WidgetSpec (e.g. a LabeledSection
      // "card"); the whole card is one select hit. Overrides `items`.
      el.classList.add("w-list-cards");
      specs.forEach((s,i)=>{
        const card=aria(div("w-list-card"+(i===sel?" sel":"")), "option", {selected:i===sel});
        card.appendChild(widgetNode(s, ctx));
        card.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeListItem(ctx,wkey,spec.itemKeys,i,e); };
        el.appendChild(card);
      });
    } else {
      (spec.items||[]).forEach((it,i)=>{
        const row=aria(div("w-list-row"+(i===sel?" sel":"")), "option", {selected:i===sel});
        fillEntry(row, it);
        row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeListItem(ctx,wkey,spec.itemKeys,i,e); };
        el.appendChild(row);
      });
      if(!(spec.items||[]).length){ const empty=div("w-list-empty"); empty.textContent="(empty)"; el.appendChild(empty); }
    }
    return el;
  }
  if(spec.kind==="tree"){
    // Same semantics as the TUI's render_widget_tree: the host-owned
    // instance state (selection, expanded set) is authoritative, a node is
    // visible iff every ancestor is expanded, and rows fire the identical
    // select / expand hits. Rendered natively (all visible rows, no TUI
    // scroll window) — off-window clicks are synthesized bridge-side.
    const el=aria(div("w-tree"+(focused?" focus":"")), "tree", {});
    const inst=instOf(ctx, wkey);
    const sel=inst&&inst.selectedIndex!=null?inst.selectedIndex:(spec.selectedIndex!=null?spec.selectedIndex:-1);
    // Host parity (first_visible_tree_index): once an INSTANCE exists its
    // expansion set is authoritative even when EMPTY; the spec's seed applies
    // only before any instance state. The scene skip-serializes an empty
    // expandedKeys, so "instance present, field absent" means all-collapsed —
    // falling back to the seed there made collapsing the last open folder a
    // visual no-op.
    const expanded=new Set(inst?(inst.expandedKeys||[]):(spec.expandedKeys||[]));
    const keys=spec.itemKeys||[];
    // Each depth's open flag down to the current node (a skipped depth counts
    // as open).
    /** const openAt: Boolean[] */
    const openAt=[];
    (spec.nodes||[]).forEach((n,i)=>{
      const d=n.depth||0;
      while(openAt.length>d) openAt.pop();
      while(openAt.length<d) openAt.push(true);
      const vis=openAt.every(b=>b);
      const key=keys[i]||"";
      const isOpen=n.hasChildren?(!!key&&expanded.has(key)):true;
      openAt.push(isOpen);
      if(!vis) return;
      const row=aria(div("w-tree-row"+(i===sel?" sel":"")), "treeitem",
        {selected:i===sel, level:d+1, expanded:n.hasChildren?isOpen:null,
         checked:spec.checkable&&n.checked!=null?!!n.checked:null});
      row.style.paddingLeft=`${d*2}ch`;
      // Right-click anywhere on the row (disclosure/checkbox included, like
      // the TUI's cell hit-test) fires `context` instead of the cell's own
      // event — the plugin raises a context menu anchored at the click cell.
      const treeCtx=e=>{ routeTree(ctx,wkey,"context",{index:i,key},cellAt(e)); };
      if(n.hasChildren){
        const g=document.createElement("span"); g.className="w-tree-disc"; g.textContent=isOpen?"▼ ":"▶ ";
        g.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,wkey,"expand",{index:i,key,expanded:!isOpen},null); };
        row.appendChild(g);
      }
      if(spec.checkable&&n.checked!=null){
        const cb=document.createElement("span"); cb.className="w-tree-check"; cb.textContent=n.checked?"☑ ":"☐ ";
        cb.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,wkey,"toggle",{index:i,key,checked:!n.checked},null); };
        row.appendChild(cb);
      }
      const t=document.createElement("span"); t.className="w-tree-text"; fillEntry(t, n.text);
      row.appendChild(t);
      // The row's action button (TreeNode.action), the web twin of the TUI's
      // `[ label ]` at the row's tail: its own target, so a press on it is the
      // button's and not the row's select.
      if(n.action){
        const a=document.createElement("span"); a.className="w-tree-action"; a.textContent="[ "+n.action+" ]";
        a.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e);
          else { routeTree(ctx,wkey,"select",{index:i,key},null); routeTree(ctx,wkey,"action",{index:i,key},null); } };
        row.appendChild(a);
      }
      // A click anywhere on a FOLDER row both selects it and toggles its
      // expansion — the disclosure glyph alone is a needle-thin target, and
      // "click a folder to fold it" is what every tree UI trains people to
      // expect. Leaf rows keep plain select.
      row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e);
        else { routeTree(ctx,wkey,"select",{index:i,key},null);
               if(n.hasChildren) routeTree(ctx,wkey,"expand",{index:i,key,expanded:!isOpen},null); } };
      // Bordered card node (Tree.cardBorders + extraLines): wrap the
      // primary row and its continuation rows in a rounded box, the web
      // twin of the TUI's `╭─…─╮` card chrome. Non-card nodes (folder
      // headers) stay plain rows.
      const isCard=!!spec.cardBorders&&!n.hasChildren&&(n.extraLines||[]).length>0;
      const holder=isCard?div("w-tree-card"+(i===sel?" sel":"")):el;
      if(isCard){ holder.style.marginLeft=`${d*2}ch`; row.style.paddingLeft="0"; }
      holder.appendChild(row);
      // Fixed-height card rows (Tree.itemHeight > 1): the node's
      // extraLines render as continuation rows below the primary line,
      // indented past the disclosure column. The whole card is one
      // click target and highlights as a block — same contract as the
      // TUI, which emits a select hit for every continuation row.
      for(const ex of (n.extraLines||[])){
        const xr=div("w-tree-xrow"+(i===sel?" sel":""));
        xr.style.paddingLeft=isCard?"2ch":`${d*2+2}ch`;
        fillEntry(xr, ex);
        xr.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e); else routeTree(ctx,wkey,"select",{index:i,key},null); };
        holder.appendChild(xr);
      }
      if(isCard) el.appendChild(holder);
    });
    return el;
  }
  if(spec.kind==="overlay"){
    // The TUI floats the child over the following rows; here it renders as
    // a raised dropdown card in flow, right under the row that opened it.
    const el=div("w-overlay");
    if(spec.child) el.appendChild(widgetNode(spec.child, ctx));
    return el;
  }
  if(spec.kind==="popup"){
    // A popup layer — renders like overlay here (raised card in flow);
    // the TUI floats it over the panel rows.
    const el=div("w-overlay");
    if(spec.child) el.appendChild(widgetNode(spec.child, ctx));
    return el;
  }
  if(spec.kind==="component"){
    // A focus/event scope — purely behavioural in the TUI; renders its
    // child transparently here.
    if(spec.child) return widgetNode(spec.child, ctx);
    return div("w-component-empty");
  }
  if(spec.kind==="raw"&&spec.entries){
    const text=spec.entries.map(e=>entryText(e)).join("\n");
    if(ctx.table){
      // A table cell: a rule, a caption, or air.
      const tt=text.trim();
      if(/^[│┃|]$/.test(tt)) return div("w-vrule");
      const cell=div(tt!==""&&tt===tt.toUpperCase()&&/[A-Z]/.test(tt)?"w-th":"w-td"); cell.textContent=tt; return cell;
    }
    // Blank rows a terminal layout stacks to push what follows to the bottom
    // of the panel: a flexible gap here, which does the same at any height.
    if(!ctx.grid&&!text.trim()){
      // In a row, spaces are a gap as wide as they are; in a column, blank
      // rows push what follows down.
      if(ctx.inRow){ const g=div("w-gap"); g.style.width=`${spec.entries.reduce((m,e)=>Math.max(m,entryText(e).length),1)*0.6}ch`; return g; }
      return div("w-fill");
    }
    const el=div("w-raw");
    spec.entries.forEach((e,i)=>{ if(i) el.append("\n"); fillEntry(el.appendChild(document.createElement("span")), e); });
    return el;
  }
  if(spec.kind==="windowEmbed"){
    // Embedding another editor window's live cells inside a native panel
    // needs its own scene region (the webui cell buffer carries pane
    // interiors only) — honest placeholder until that lands.
    const el=div("w-embed-ph");
    el.textContent="⧉ live preview is not available in the web UI yet";
    if(spec.rows) el.style.minHeight=`${spec.rows*CH}px`;
    return el;
  }
  const fb=div("w-unsupported"); fb.textContent="["+kind+"]"; return fb;
}

// A tree row press by identity: the tree's key, the event, the row.
// `cell`: the cell a right-click landed on (a `context` press), else null.
/** function routeTree<T>(WCtx, String | Undefined, String, T, {col: Number, row: Number} | Null) => Undefined */
function routeTree(ctx, treeKey, eventType, payload, cell){
  if(ctx.kind!=="panel") return;
  const msg={...panelBase(ctx),widgetKey:treeKey||"",eventType,payload};
  if(cell) sendWidget({...msg,col:cell.col,row:cell.row});
  else sendWidget(msg);
}

// Native Settings — a control's value rendered semantically (read from the
// shared model; keyboard drives edits via handle_key).
const SET_LIST_KINDS = new Set(["textList","map","objectArray","dualList"]);
// Inline control (toggle/number/dropdown/text/json/complex). `idx` = item index;
// `live` enables click routing (false inside the entry dialog, which is keyboard).
/** function settingControlEl(SettingControl, Number, Boolean) => El */
function settingControlEl(c, idx, live){
  const k=c.kind, el=document.createElement("span"); el.className="set-ctl set-"+k;
  if(c.kind==="toggle"){ const sw=document.createElement("span"); sw.className="set-switch"+(c.checked?" on":""); aria(sw,"switch",{checked:!!c.checked}); el.appendChild(sw); if(live) sw.onmousedown=setHit("controlToggle",idx,undefined); }
  else if(c.kind==="number"){
    const mk=(t,cls,hit)=>{const b=document.createElement("span");b.className=cls;b.textContent=t;if(live&&hit!=="")b.onmousedown=setHit(hit,idx,undefined);return b;};
    // The value as the JSON carries it, shown the way the TUI's cell shows it:
    // a percent ×100, an integer whole.
    const disp=c.percent?`${Math.round(c.value*100)}%`:c.integer?String(Math.round(c.value)):String(Number(toFixed(c.value, 3)));
    el.appendChild(mk("−","set-step","controlDecrement")); const v=document.createElement("span"); v.className="set-num-v"; v.textContent=disp; el.appendChild(v); el.appendChild(mk("+","set-step","controlIncrement"));
  }
  else if(c.kind==="dropdown"){ const p=document.createElement("span"); p.className="set-pill"; p.textContent=(c.options[c.selected]||"—")+" ▾"; if(live) p.onmousedown=setHit("controlDropdown",idx,undefined); el.appendChild(p);
    const sel=c.selected;
    if(c.open){ const d=div("set-dd"); c.options.forEach((o,i)=>{const r=div("set-dd-row"+(i===sel?" sel":""));r.textContent=o;if(live)r.onmousedown=setHit("controlDropdownOption",idx,i);d.appendChild(r);});
      // Portal to the body-level host (same as the plugin Dropdown): the
      // Settings dialog is a transformed, overflow-clipping box with a
      // scrolling list, so an in-place list is cut off at the dialog edge (a
      // long option set — e.g. Default Language — is far wider than the compact
      // pill). Out of the modal it grows to fit the widest option and shifts to
      // stay in view (right-aligning under the flush-right control).
      mountPopover(d);
      requestAnimationFrame(()=>positionFloatingPopover(p, d, {align:"end"})); } }
  else if(c.kind==="text"){ const f=document.createElement("span"); f.className="set-field"; f.textContent=(c.value||c.placeholder||"")+(c.editing?"▌":""); if(live) f.onmousedown=setHit("controlText",idx,undefined); el.appendChild(f); }
  else if(c.kind==="json"){ const f=document.createElement("span"); f.className="set-field mono"; f.textContent=(c.value||"").slice(0,80)||"{}"; if(live) f.onmousedown=setHit("controlText",idx,undefined); el.appendChild(f); }
  // The field is `type_name`: the enum's rename_all renames variants, not fields.
  else if(c.kind==="complex"){ el.textContent=`‹${c.type_name}›`; el.classList.add("set-dim"); }
  return el;
}
// Full-width list block for composite controls (rows + an Add affordance).
/** function settingListEl(SettingControl, Number, Boolean) => El */
function settingListEl(c, idx, live){
  const k=c.kind;
  // DualList has its own two-column layout (Available | Included) plus the
  // add/remove/move buttons; render it natively and route each click to the
  // matching SettingsHit (same dispatch the TUI uses).
  if(c.kind==="dualList") return dualListEl(c, idx, live);
  const el=div("set-list");
  /** let rows: {label: String, sub: String, focused: Boolean}[] */
  let rows=[];
  // rowHit: the SettingsHit kind for clicking a row (b = the row); addHit/addB
  // the hit for "＋ Add" ("" = none).
  let rowHit="", addHit="";
  /** let addB: Number | Undefined */
  let addB=undefined;
  if(c.kind==="textList"){ const f=c.focused; rows=c.items.map((t,i)=>({label:t, sub:"", focused:i===f})); rowHit="controlTextListRow"; addHit="controlTextListRow"; addB=c.items.length; }
  else if(c.kind==="map"){ const f=c.focused; rows=c.entries.map((e,i)=>({label:e.key, sub:e.display, focused:i===f})); rowHit="controlMapRow"; if(!c.noAdd) addHit="controlMapAddNew"; }
  else if(c.kind==="objectArray"){ const f=c.focused; rows=c.entries.map((t,i)=>({label:t, sub:"", focused:i===f})); rowHit="controlMapRow"; }   // select item; Enter edits
  // Column header (`Name │ <column>`), mirroring the TUI's dimmed header row.
  const column=c.kind==="map"?c.column:null;
  if(column!=null&&column!==""&&rows.length>0){ const h=div("set-list-row set-list-head");
    const l=document.createElement("span"); l.className="set-list-label"; l.textContent="Name"; h.appendChild(l);
    const s=document.createElement("span"); s.className="set-list-sub"; s.textContent=column; h.appendChild(s);
    el.appendChild(h); }
  if(!rows.length){ const e=div("set-list-empty"); e.textContent="No entries"; el.appendChild(e); }
  rows.forEach((r,i)=>{ const row=div("set-list-row"+(r.focused?" sel":""));
    const l=document.createElement("span"); l.className="set-list-label"; l.textContent=r.label; row.appendChild(l);
    if(r.sub){ const s=document.createElement("span"); s.className="set-list-sub"; s.textContent=r.sub; row.appendChild(s); }
    if(r.focused&&(k==="map"||k==="objectArray")){ const h=document.createElement("span"); h.className="set-list-hint"; h.textContent="[Enter to edit]"; row.appendChild(h); }
    if(live&&rowHit!=="") row.onmousedown=setHit(rowHit,idx,i);
    el.appendChild(row); });
  // Auto-managed maps (noAdd, e.g. Languages/LSP) hide the add row like the TUI.
  // (A text list has no add-row focus.)
  const addSel=c.kind==="map"?c.addFocused:c.kind==="objectArray"?c.addFocused:false;
  if(!(c.kind==="map"&&c.noAdd)){ const add=div("set-list-add"+(addSel?" sel":"")); add.textContent="＋ Add…"; if(live&&addHit!=="") add.onmousedown=setHit(addHit,idx,addB); el.appendChild(add); }
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
  const mkb=(t,hit,title)=>{ const b=div("set-dual-btn"); b.textContent=t; b.title=title; if(live) b.onmousedown=setHit(hit,idx,undefined); return b; };
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
/** function settingItemRows(SettingItem[], El, Boolean, Boolean) => Undefined */
function settingItemRows(items, container, live, entry){
  for(const it of items){
    const sec=it.section;
    if(it.sectionStart && sec!=null && sec!==""){ const h=div("set-section"); h.textContent=sec; container.appendChild(h); }
    const isList=SET_LIST_KINDS.has(it.control.kind);
    const row=aria(div("set-item"+(it.selected?" sel":"")+(it.readOnly?" ro":"")+(isList?" set-item-block":"")),
      "group", {label:it.name, current:it.selected?"true":null, readonly:it.readOnly||null});
    const head=div("set-item-head");
    const nm=div("set-name"); nm.textContent=it.name+(it.modified?" •":""); head.appendChild(nm);
    const selectHit = entry
      ? (it.readOnly ? null : e=>{ e.preventDefault(); e.stopPropagation(); sendSettings({kind:"entryItem", a:it.index}); })
      : (live ? setHit("item",it.index,undefined) : null);  // click the row label = select item
    if(selectHit){ nm.onmousedown=selectHit; }
    if(!isList){ const ctl=div("set-ctl-wrap"); ctl.appendChild(settingControlEl(it.control,it.index,live&&!entry));
      for(const named of ctl.querySelectorAll("[role]")) named.setAttribute("aria-label", it.name); head.appendChild(ctl); if(entry&&selectHit) ctl.onmousedown=selectHit; }
    row.appendChild(head);
    const desc=it.description;
    if(desc){ const d=div("set-desc"); d.textContent=desc; row.appendChild(d); }
    if(isList) row.appendChild(settingListEl(it.control,it.index,live&&!entry));
    container.appendChild(row);
  }
}
/** function settingsEls(Settings) => El[] */
function settingsEls(s){
  const out=[]; const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim);
  const el=aria(div("region settings-modal"), "dialog", {modal:true, label:s.title});
  const title=div("set-title"); title.textContent=s.title+"  —  layer: "+s.targetLayer; el.appendChild(title);
  const body=div("set-body");
  // left: category tree
  const cats=aria(div("set-cats"+(s.focus==="categories"?" focus":"")), "tree", {label:"Categories"});
  for(const c of s.categories){ const r=aria(div("set-cat"+(c.selected?" sel":"")), "treeitem",
      {selected:!!c.selected, level:c.nested?2:1, expanded:c.expandable?!!c.expanded:null, label:c.name});
    if(c.nested) r.appendChild(document.createTextNode("    "));
    else if(c.expandable){ const chev=document.createElement("span"); chev.className="set-cat-chev"; chev.textContent=c.expanded?"▼ ":"▶ "; chev.onmousedown=setHit("categoryDisclosure",c.index,undefined); r.appendChild(chev); } else r.appendChild(document.createTextNode("  "));
    const nm=document.createElement("span"); nm.textContent=c.name; r.appendChild(nm);
    r.onmousedown=setHit("category",c.index,undefined); cats.appendChild(r);
    if(c.expanded){ c.sections.forEach((sec,si)=>{ const sr=aria(div("set-cat-sec"),"treeitem",{selected:false,level:2,label:sec}); sr.textContent="   "+sec; sr.onmousedown=setHit("categorySection",c.index,si); cats.appendChild(sr); }); } }
  body.appendChild(cats);
  // right: items, or search results
  const right=div("set-items"+(s.focus==="settings"?" focus":""));
  if(s.searchActive){ const sb=div("set-search"); sb.textContent="Search: "+s.searchQuery+"▌"; right.appendChild(sb);
    s.searchResults.forEach((r,i)=>{const rr=div("set-sresult"+(i===s.searchSelected?" sel":""));rr.textContent=r.name+"   — "+r.category;rr.onmousedown=setHit("searchResult",i,undefined);right.appendChild(rr);}); }
  else { settingItemRows(s.items, right, true, false); }
  body.appendChild(right);
  el.appendChild(body);
  // footer
  const ft=div("set-footer"); const fkinds=["layer","reset","save","cancel"];
  s.footerButtons.forEach((b,i)=>{
    const sel=i===s.footerSelected&&s.focus==="footer";
    const bb=aria(div("btn"+(b==="Save"?" primary":"")+(sel?" sel":"")),"button",{}); bb.textContent=b; bb.onmousedown=setHit(fkinds[i]||"cancel",undefined,undefined); ft.appendChild(bb);
  }); el.appendChild(ft);
  // entry dialog (Map/ObjectArray add/edit): rows are clickable (select+edit),
  // controls keyboard-driven, buttons routed by name to the entry dispatch.
  const d=s.entryDialog;
  if(d){ const dlg=aria(div("set-overlay set-entry"),"dialog",{modal:true,label:d.title});
    const dt=div("set-entry-title"); dt.textContent=d.title; dlg.appendChild(dt);
    const di=div("set-entry-items"); settingItemRows(d.items.map((it,i)=>({...it,selected:i===d.selectedItem&&!d.focusOnButtons})), di, true, true); dlg.appendChild(di);
    const db=div("set-dlg-btns");
    // Button focus index follows the TUI order [Save, Cancel, Delete]; the
    // visual order here puts Delete in the middle but each routes by name.
    /** const tuiIdx: Dict<Number> */
    const tuiIdx={Save:0,Cancel:1,Delete:2};
    const btns=d.noDelete?["Save","Cancel"]:["Save","Delete","Cancel"];
    btns.forEach(b=>{const f=d.focusOnButtons&&tuiIdx[b]===d.focusedButton; const bb=aria(div("btn"+(b==="Save"?" primary":b==="Delete"?" danger":"")+(f?" sel":"")),"button",{}); bb.textContent=b;
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
/** function keybindingEditorEls(KeybindingEditor) => El[] */
function keybindingEditorEls(kb){
  const out=[];
  const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim);
  const el=div("region kbedit");
  const title=div("kb-title"); title.textContent=kb.title; el.appendChild(title);

  // header: config path + search + filters
  const hdr=div("kb-header");
  const cfg=div("kb-cfg"); cfg.textContent="Config: "+kb.configPath+(kb.keymaps.length>0?("   Maps: "+kb.keymaps.join(", ")):""); hdr.appendChild(cfg);
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
    if(r.type==="section"){ const row=div("kb-row kb-section"+(r.selected?" sel":"")); row.textContent=`${r.collapsed?"▶ ":"▼ "}${r.name} (${r.count})`; row.onmousedown=kbHit(idx); tbl.appendChild(row); continue; }
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
  const d=kb.editDialog;
  if(d){
    const dlg=div("kb-overlay kb-dialog");
    const dt=div("kb-dlg-title"); dt.textContent=d.title; dlg.appendChild(dt);
    const fld=(label,val,focused,extra)=>{ const f=div("kb-field"+(focused?" focus":"")); const l=document.createElement("span"); l.className="kb-field-l"; l.textContent=label; f.appendChild(l);
      const v=document.createElement("span"); v.className="kb-field-v"; v.textContent=val; f.appendChild(v); if(extra){ const e2=document.createElement("span"); e2.className="kb-field-x"; e2.textContent=extra; f.appendChild(e2);} return f; };
    dlg.appendChild(fld("Key:", d.keyDisplay||(d.keyCapturing?"Press a key…":"(none)"), d.focusArea===0, d.keyCapturing?"(capturing)":""));
    const af=fld("Action:", d.actionText+(d.focusArea===1?"▌":""), d.focusArea===1, "");
    const aerr=d.actionError;
    if(aerr){ const e2=document.createElement("span"); e2.className="kb-err"; e2.textContent="✗ "+aerr; af.appendChild(e2); }
    dlg.appendChild(af);
    if(d.autocomplete.length>0){ const ac=div("kb-autocomplete");
      d.autocomplete.forEach((s,i)=>{ const r=div("kb-ac-row"+(i===d.autocompleteSelected?" sel":"")); r.textContent=s; ac.appendChild(r); }); dlg.appendChild(ac); }
    dlg.appendChild(fld("Context:", "["+d.context+"]", d.focusArea===2, d.focusArea===2?"(← → cycle)":""));
    if(d.conflicts.length>0){ const cf2=div("kb-conflicts"); cf2.innerHTML="<b>Conflicts:</b>"; for(const c of d.conflicts){ const cr=document.createElement("div"); cr.textContent="  "+c; cf2.appendChild(cr);} dlg.appendChild(cf2); }
    const btns=div("kb-dlg-btns");
    const sb=div("kb-btn"+(d.saveFocused?" focus":"")); sb.textContent="[ Save ]"; btns.appendChild(sb);
    const cb=div("kb-btn"+(d.cancelFocused?" focus":"")); cb.textContent="[ Cancel ]"; btns.appendChild(cb);
    dlg.appendChild(btns);
    el.appendChild(dlg);
  }

  // confirm dialog
  const kc=kb.confirm;
  if(kc){ const cbox=div("kb-overlay kb-confirm");
    const m=div("kb-confirm-msg"); m.textContent="Save changes before closing?"; cbox.appendChild(m);
    const b=div("kb-dlg-btns"); kc.buttons.forEach((label,i)=>{ const bb=div("kb-btn"+(i===kc.selected?" focus":"")); bb.textContent="[ "+label+" ]"; b.appendChild(bb); }); cbox.appendChild(b);
    el.appendChild(cbox);
  }

  out.push(el);
  return out;
}

// Native auxiliary modals (keybinding editor / event-debug / theme-info). A
// titled list of lines; anchored (theme-info) or centered. Interaction (nav /
// Esc / rebind) already flows through handle_key, so this is render-only.
/** function auxModalEls(AuxModal) => El[] */
function auxModalEls(m){
  const out=[];
  const rect=m.rect;
  const anchored = rect!==null && (rect.x!==0 || rect.y!==0);
  if(!anchored){ const scrim=div("modal-scrim"); scrim.onmousedown=e=>e.stopPropagation(); out.push(scrim); }
  const el=div("region auxmodal "+(anchored?"anchored":"centered")+" am-"+m.kind);
  if(rect!==null&&anchored){ el.style.left=`${px(rect.x,CW)}px`; el.style.top=`${px(rect.y,CH)}px`; }
  const title=div("am-title"); title.textContent=m.title; el.appendChild(title);
  const body=div("am-body");
  for(const ln of m.lines){ const r=div("am-line"+(ln.selected?" sel":"")); r.textContent=ln.text||" "; body.appendChild(r); }
  el.appendChild(body);
  const footer=m.footer;
  if(footer){ const f=div("am-footer"); f.textContent=footer; el.appendChild(f); }
  out.push(el);
  return out;
}

// Native right-click / new-tab context menu. Rendered at the menu's cell
// position; clicking item i forwards a click at (x+1, y+1+i) — the cell the
// editor's hit-test resolves to that item (`item_idx = row - y - 1`).
/** function contextMenuEl(ContextMenu) => El */
function contextMenuEl(cm){
  const el=div("region ctxmenu");
  el.style.left=`${px(cm.x,CW)}px`; el.style.top=`${px(cm.y,CH)}px`;
  cm.items.forEach((label,i)=>{
    const row=div("ctxitem"+(i===cm.highlighted?" sel":""));
    row.textContent=label;
    const cell={col:cm.x+1,row:cm.y+1+i};
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:cell.col,row:cell.row}); };
    row.onmouseenter=()=>sendMouse({kind:"moved",col:cell.col,row:cell.row});
    el.appendChild(row);
  });
  return el;
}

