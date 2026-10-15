// Plugin widget tree (WidgetSpec) + Settings / keybinding editor / aux modals.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native plugin widgets (WidgetSpec tree) ----------------------------
// Renders the serialized WidgetSpec natively (Row/Col→flex, Toggle/Button→
// native controls, …). Toggle/Button clicks route back to the owning plugin
// via the same event the TUI fires; `surface` selects the routing path.
// Concatenate a TextPropertyEntry's display text (segments preferred).
function entryText(e){ return (e.segments&&e.segments.length)?e.segments.map(s=>s.text||"").join(""):(e.text||""); }
// A style's look, as the host resolved its colours (`fgCss`/`bgCss`, from the
// theme keys a plugin names) — applied inline to the span it covers.
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
function fillEntry(el, e){
  if(!e) return el;
  applyStyle(el, e.style);
  if(e.segments&&e.segments.length){
    for(const sg of e.segments){ const sp=document.createElement("span"); sp.textContent=sg.text||""; applyStyle(sp, sg.style); el.appendChild(sp); }
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
function labelColon(text){
  const t=(text||"").replace(/\s+$/,"");
  return /[:：]$/.test(t)?t+" ":t+": ";
}
// Accessibility: each control says what it is (role), what it is called
// (name) and where it stands (state), so assistive tech — and tests, by role
// and name — find it as the control it draws. Nothing takes the keyboard
// (the editor owns every key), so there is no tabindex here.
function aria(el, role, attrs){
  if(role) el.setAttribute("role", role);
  for(const k in (attrs||{})){
    const v=attrs[k];
    if(v==null||v===false&&k!=="checked"&&k!=="selected"&&k!=="expanded") continue;
    el.setAttribute("aria-"+k, String(v));
  }
  return el;
}
function formLabel(text, labelWidth){
  const l=document.createElement("span");
  l.className="w-flabel";
  l.textContent=labelColon(text);
  if(labelWidth>0) l.style.setProperty("--flw", labelWidth);
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
function panelBase(ctx){
  const b={surface:"panel",slot:ctx.slot};
  if(ctx.section!=null) b.section=ctx.section;
  if(ctx.pane!=null) b.pane=ctx.pane;
  return b;
}
function routeWidget(ctx, node, listIndex, ev){
  if(ctx.kind==="toolbar"){ if(node.key) sendWidget({surface:"toolbar",key:node.key}); return; }
  const base=panelBase(ctx);
  if(listIndex!=null){
    const itemKey=(node.itemKeys&&node.itemKeys[listIndex]!=null)?node.itemKeys[listIndex]:"";
    const payload={index:listIndex,key:itemKey,list_key:node.key||null};
    // Right-click raises the plugin's context menu, anchored at the cell.
    if(ev&&ev.button===2){
      const c=cellAt(ev);
      sendWidget({...base,widgetKey:itemKey,eventType:"context",payload,col:c.col,row:c.row});
      return;
    }
    sendWidget({...base,widgetKey:itemKey,eventType:"select",payload,clicks:(ev&&ev.detail)||1});
    return;
  }
  if(node.key){
    const et=node.kind==="button"?"activate":node.kind==="toggle"?"toggle":node.kind==="text"?"focus":null;
    if(et) sendWidget({...base,widgetKey:node.key,eventType:et,payload:{}});
  }
}

// A control press by key + the specific event (a dropdown answers both
// `dropdown_toggle` and `dropdown_select`).
function routeControl(ctx,key,eventType,payload){
  if(ctx.kind!=="panel") return;
  sendWidget({...panelBase(ctx),widgetKey:key,eventType,payload:payload||{}});
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

// A single body-level host for dropdown option lists. Portaling the list OUT
// of its modal is what lets it never be clipped: a floating modal / the
// Settings dialog is a `transform`ed, `overflow`-clipping box, so ANY
// descendant (even `position:fixed`) is clipped to it. As a child of <body>
// the list has the viewport as its containing block and nothing clips it — the
// approach every portal-based design system (Radix, MUI, …) uses. The host is
// 0×0 (its children are all fixed-positioned), so it never intercepts clicks.
function popoverHost(){
  let h=document.getElementById("fresh-popover-host");
  if(!h){ h=document.createElement("div"); h.id="fresh-popover-host"; document.body.appendChild(h); }
  return h;
}
// Mount a dropdown option list into the body-level host, tagged with the region
// currently rendering (renderRegion sets `popoverRegionOwner`). That region
// reaps its own tagged pop-overs on its next fill, so the list disappears the
// moment the dropdown closes and can never leak/accumulate across frames.
function mountPopover(el){
  el.dataset.popoverRegion=(typeof popoverRegionOwner!=="undefined"&&popoverRegionOwner)||"";
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
function positionFloatingPopover(anchor, popup, opts){
  if(!anchor||!anchor.isConnected||!popup) return;
  opts=opts||{};
  // Fixed-positioning containing block: the viewport, unless an ancestor
  // establishes one (transform / filter / backdrop-filter / perspective /
  // will-change). Portaled popovers live under <body> so this is normally the
  // viewport; the walk stays for robustness (and for any non-portaled caller).
  let cb={left:0,top:0,right:window.innerWidth,bottom:window.innerHeight};
  for(let el=popup.parentElement; el; el=el.parentElement){
    const cs=getComputedStyle(el);
    if(cs.transform!=="none"||cs.perspective!=="none"
       ||(cs.filter&&cs.filter!=="none")
       ||(cs.backdropFilter&&cs.backdropFilter!=="none")
       ||(cs.webkitBackdropFilter&&cs.webkitBackdropFilter!=="none")
       ||/transform|filter|perspective/.test(cs.willChange||"")){
      cb=el.getBoundingClientRect(); break;
    }
  }
  const a=anchor.getBoundingClientRect();
  popup.style.minWidth=a.width+"px";       // >= trigger, grow-to-content past it
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
  popup.style.left=(left-cb.left)+"px";
  popup.style.top=(top-cb.top)+"px";
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

// Lines a terminal layout wrapped to its measure, one row per line, read as
// one paragraph natively: a run of rows that each hold just a bare full-width
// button with the same press target and style (a card's description) becomes
// one button whose text the browser wraps to the card. Rows of other targets,
// and the empty lines between paragraphs, are left as they are.
function paragraphs(kids){
  const line=c=>{
    const b=c&&c.kind==="row"&&(c.children||[]).length===1?c.children[0]:null;
    return b&&b.kind==="button"&&b.bare&&b.fullWidth&&(b.label||"").trim()?b:null;
  };
  const out=[];
  for(let i=0;i<kids.length;i++){
    const first=line(kids[i]);
    if(!first){ out.push(kids[i]); continue; }
    const same=b=>b&&b.key===first.key&&b.focusable===first.focusable
      &&JSON.stringify(b.style||null)===JSON.stringify(first.style||null);
    let j=i+1, text=first.label.trim();
    while(j<kids.length&&same(line(kids[j]))){ text+=" "+line(kids[j]).label.trim(); j++; }
    out.push(j>i+1?{...first,label:text,paragraph:true}:kids[i]);
    i=j-1;
  }
  return out;
}

// Every native control carries its widget key (`data-wkey`): a page finds
// its widgets by it to scroll as the editor does (`syncPage`, 73-native.js).
function widgetEl(spec, ctx){
  const el=widgetElOf(spec, ctx);
  if(spec&&spec.key&&el&&el.nodeType===1&&!el.dataset.wkey) el.dataset.wkey=spec.key;
  return el;
}
function widgetElOf(spec, ctx){
  if(!spec) return document.createTextNode("");
  const kind=spec.kind, fk=ctx.focusKey;
  const focused = spec.key && spec.key===fk;
  if(kind==="row"||kind==="col"){
    // A row that lays text out in cells (`raw` cells between its controls,
    // labels padded with spaces to a column) only lines up in the buffer's
    // font, so it is drawn in it: a GRID row, whose controls still answer as
    // controls. Every other row is laid out by the browser.
    const kids=spec.children||[];
    // A `raw` cell with text in it is cell-drawn content; one of only spaces
    // is a gap, and does not make its row a grid.
    const inked=x=>x&&x.kind==="raw"&&(x.entries||[]).some(e=>entryText(e).trim()!=="");
    const isGridRow=c=>c&&c.kind==="row"&&(c.children||[]).some(inked);
    // A column of grid rows that all have the same number of cells is a
    // TABLE the terminal drew line by line (a menu's two columns of entries
    // with a rule between them, Tab walking it in reading order). Natively
    // it is one CSS grid: the columns line up in any font, the cells are
    // laid out as text and controls, and a `│` cell becomes a hairline.
    if(ctx.native&&!ctx.grid&&kind==="col"&&kids.length>1&&kids.every(isGridRow)
       &&kids.every(c=>c.children.length===kids[0].children.length)){
      const el=div("w-col w-table");
      el.style.gridTemplateColumns="repeat("+kids[0].children.length+",auto)";
      const tctx={...ctx,table:true};
      for(const r of kids){
        const re=div("w-row w-table-row");
        for(const c of r.children) re.appendChild(widgetEl(c, tctx));
        el.appendChild(re);
      }
      return el;
    }
    const grid=!ctx.table&&(ctx.grid||(kind==="row"&&kids.some(inked)));
    const el=div("w-"+kind+(spec.wrap?" wrap":"")+(grid&&!ctx.grid?" grid":"")+(spec.justifyEnd?" end":""));
    const cctx={...ctx,grid:grid||ctx.grid,inRow:kind==="row"};
    for(const c of (ctx.native&&!grid&&kind==="col"?paragraphs(kids):kids)) el.appendChild(widgetEl(c, cctx));
    return el;
  }
  if(kind==="label"){
    // A terminal section heading: a caption followed by a rule of box-drawing
    // characters long enough to reach any edge. Natively, the caption and a
    // drawn rule that ends where the panel does.
    const heading=!ctx.grid&&!(spec.segments&&spec.segments.length)
      &&/^\s*[─━═]*\s*([^─━═]*?)\s*[─━═]{3,}\s*$/.exec(spec.text||"");
    if(heading){
      const el=div("w-heading");
      const t=document.createElement("span"); t.textContent=heading[1]; el.appendChild(t);
      el.appendChild(div("w-heading-rule"));
      return el;
    }
    const el=div("w-label-row");
    const segs=spec.segments&&spec.segments.length?spec.segments:[{text:spec.text||""}];
    applyStyle(el, spec.style);
    for(const sg of segs){ const sp=document.createElement("span"); sp.textContent=sg.text||""; applyStyle(sp, sg.style); el.appendChild(sp); }
    if(spec.labelWidth>0) el.style.paddingLeft=spec.labelWidth+"ch";
    if(spec.wrap) el.classList.add("wrap");
    return el;
  }
  if(kind==="toggle"){
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
    if(spec.key) el.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec); };
    return el;
  }
  if(kind==="button"){
    const el=div("w-button "+(spec.intent||"normal")+(spec.disabled?" disabled":"")+(focused?" focus":"")
      +(spec.bare?" bare":"")+(spec.fullWidth?" full":""));
    // A terminal label carries its own padding (a bare title bar's trailing
    // spaces fill the row to the close box); a laid-out button has a box. In
    // a grid row the padding IS the layout, so it stays.
    const label=ctx.grid?(spec.label||""):(spec.label||"").trim();
    if(ctx.native&&!ctx.grid&&spec.bare){
      // A bare full-width line of text is a line of the page: an empty one is
      // the gap between paragraphs, a run of them one paragraph (`paragraphs`),
      // and the spaces leading one are its indent, not centring.
      if(!label&&spec.fullWidth) return div("w-para-gap");
      if(spec.paragraph) el.classList.add("para");
      const indent=(spec.label||"").length-(spec.label||"").trimStart().length;
      if(indent&&spec.fullWidth) el.style.paddingLeft="calc("+indent*CW+"px + .35em)";
    }
    // A menu entry that says what it sets: `(•)`/`( )` a choice, `[✓]`/`[ ]`
    // a switch — drawn as the marks they stand for.
    const mark=!ctx.grid&&ctx.native&&/^(\(•\)|\( \)|\[✓\]|\[ \])\s+(.*)$/.exec(label);
    if(mark){
      const radio=mark[1][0]==="(", on=mark[1][1]!==" ";
      const m=document.createElement("span");
      m.className=(radio?"w-mark-radio":"w-mark-check")+(on?" on":"");
      el.appendChild(m); el.append(mark[2]);
    } else el.textContent=label;
    if(ctx.grid) el.classList.add("gridcell");
    if(mark) aria(el, mark[1][0]==="("?"menuitemradio":"menuitemcheckbox", {checked:mark[1][1]!==" ", label:mark[2].trim()});
    else aria(el, "button", {disabled:!!spec.disabled, label:label.trim()!==label?label.trim():null});
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
    // A section the page sized in cells (a card of a row of cards, a framed
    // box on the page's axis) keeps that width, so cards set side by side
    // come out alike and a box sits where the terminal puts it.
    if(ctx.native&&spec.widthCols>0){ el.classList.add("sized"); el.style.width=spec.widthCols*CW+"px"; }
    aria(el, "group", {label:spec.label||null});
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
    // Read-only markdown is something to read, not a field: a document that
    // is one fenced block (a page's code sample) is that code, in the buffer's
    // font; any other is its text, wrapped.
    if(ctx.native&&spec.readOnly&&spec.markdown){
      const fence=/^\s*```[^\n]*\n([\s\S]*?)\n?\s*(```\s*)?$/.exec(val);
      const el=document.createElement(fence?"pre":"div");
      el.className=fence?"w-code":"w-md";
      el.textContent=fence?fence[1].replace(/[ \t]+$/gm,""):val;
      return el;
    }
    // Width as the terminal sizes it: a multi-line field (or one asked to)
    // fills its row; a `fieldWidth` is a width in characters.
    const fill=spec.fullWidth||((spec.rows||1)>1&&!spec.fieldWidth);
    const el=div("w-text"+(focused?" focus":"")+(fill?" fill":""));
    if(!fill&&spec.fieldWidth>0) el.style.width="calc("+spec.fieldWidth+"ch + 1.2em)";
    // A form-style field (`label` + `labelWidth`) keeps its label OUTSIDE the
    // bordered field, in the shared label column, so the label doesn't get
    // boxed into the input and the field aligns with the other controls. A
    // plain labelled field (no width) keeps the legacy inline label.
    const formField=spec.label&&spec.labelWidth>0;
    if(spec.label&&!formField){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=labelColon(spec.label); el.appendChild(l); }
    const multi=(spec.rows||1)>1;
    const input=document.createElement(multi?"textarea":"input");
    input.className="w-text-input";
    if(multi) input.rows=spec.rows;
    input.value=val;
    input.placeholder=spec.placeholder||"";
    aria(input, null, {label:(spec.label?labelColon(spec.label).replace(/\s*:\s*$/,""):spec.placeholder)||null,
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
        sendWidget({...panelBase(ctx),widgetKey:spec.key,eventType:"focus",
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
    if(formField){
      const rowEl=div("w-field-row");
      rowEl.appendChild(formLabel(spec.label, spec.labelWidth));
      rowEl.appendChild(el);
      return rowEl;
    }
    return el;
  }
  if(kind==="radio"){
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const sel=(inst&&inst.selectedIndex!=null)?inst.selectedIndex:(spec.selectedIndex||0);
    const el=aria(div("w-radio"+(focused?" focus":"")), "radiogroup", {label:spec.label||null});
    if(spec.label) el.appendChild(formLabel(spec.label, spec.labelWidth));
    else if(spec.labelWidth>0) el.style.paddingLeft=spec.labelWidth+"ch";
    (spec.options||[]).forEach((o,i)=>{
      const opt=aria(div("w-radio-opt"+(i===sel?" on":"")), "radio", {checked:i===sel, label:o});
      opt.appendChild(div("w-radio-dot"));
      const t=document.createElement("span"); t.textContent=o; opt.appendChild(t);
      opt.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"radio_select",{index:i}); };
      el.appendChild(opt);
    });
    return el;
  }
  if(kind==="number"){
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const val=(inst&&inst.numberValue!=null)?inst.numberValue:(spec.value||0);
    const el=div("w-number"+(focused?" focus":""));
    if(spec.label){ const l=document.createElement("span"); l.className="w-text-label"; l.textContent=labelColon(spec.label); el.appendChild(l); }
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
    aria(v, "spinbutton", {valuenow:val, valuetext:disp, label:spec.label||null});
    v.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"number_value",{}); };
    el.appendChild(v);
    el.appendChild(mk("+","ArrowRight"));
    return el;
  }
  if(kind==="dropdown"){
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const selIdx=(inst&&inst.selectedIndex!=null)?inst.selectedIndex:(spec.selectedIndex||0);
    const open=inst?!!inst.dropdownOpen:!!spec.open;
    const el=div("w-dropdown"+(focused?" focus":"")+(spec.label&&spec.labelWidth>0?" wform":""));
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
    pill.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"dropdown_toggle",{}); };
    el.appendChild(pill);
    if(open){
      const dd=aria(div("w-dd w-dd-floating"), "listbox", {label:spec.label||null});
      (spec.options||[]).forEach((o,i)=>{
        const r=aria(div("w-dd-row"+(i===selIdx?" sel":"")), "option", {selected:i===selIdx}); r.textContent=o;
        r.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(spec.key) routeControl(ctx,spec.key,"dropdown_select",{index:i}); };
        dd.appendChild(r);
      });
      // Portal the option list to the body-level host so it extends PAST the
      // modal's border and is never clipped by the surface's overflow (parity
      // with the TUI popover). Shared positioner anchors it under the pill,
      // grows it to fit the widest item, and flips/shifts to stay on screen.
      mountPopover(dd);
      requestAnimationFrame(()=>positionFloatingPopover(pill, dd));
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
    const el=aria(div("w-list"), "listbox");
    const inst=(ctx.instances&&spec.key)?ctx.instances[spec.key]:null;
    const sel=inst?inst.selectedIndex:(spec.selectedIndex!=null?spec.selectedIndex:-1);
    const specs=spec.itemSpecs||[];
    if(specs.length){
      // Card list: each logical item is a WidgetSpec (e.g. a LabeledSection
      // "card"); the whole card is one select hit. Overrides `items`.
      el.classList.add("w-list-cards");
      specs.forEach((s,i)=>{
        const card=aria(div("w-list-card"+(i===sel?" sel":"")), "option", {selected:i===sel});
        card.appendChild(widgetEl(s, ctx));
        card.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); routeWidget(ctx,spec,i,e); };
        el.appendChild(card);
      });
    } else {
      (spec.items||[]).forEach((it,i)=>{
        const row=aria(div("w-list-row"+(i===sel?" sel":"")), "option", {selected:i===sel});
        fillEntry(row, it);
        // A row whose text pads its columns with spaces (a name, then its
        // kind tag at a fixed column) only lines up in the buffer's font.
        if(ctx.native&&/\S {2,}\S/.test(row.textContent)) row.classList.add("cells");
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
    const el=aria(div("w-tree"+(focused?" focus":"")), "tree");
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
      const row=aria(div("w-tree-row"+(i===sel?" sel":"")), "treeitem",
        {selected:i===sel, level:d+1, expanded:n.hasChildren?isOpen:null,
         checked:spec.checkable&&n.checked!=null?!!n.checked:null});
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
      const t=document.createElement("span"); t.className="w-tree-text"; fillEntry(t, n.text||{});
      row.appendChild(t);
      // The row's action button (TreeNode.action), the web twin of the TUI's
      // `[ label ]` at the row's tail: its own target, so a press on it is the
      // button's and not the row's select.
      if(n.action){
        const a=document.createElement("span"); a.className="w-tree-action"; a.textContent="[ "+n.action+" ]";
        a.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); if(e.button===2) treeCtx(e);
          else { routeTree(ctx,spec,"select",{index:i,key}); routeTree(ctx,spec,"action",{index:i,key}); } };
        row.appendChild(a);
      }
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
        fillEntry(xr, ex);
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
  if(kind==="popup"){
    // A popup layer — renders like overlay here (raised card in flow);
    // the TUI floats it over the panel rows.
    const el=div("w-overlay");
    if(spec.child) el.appendChild(widgetEl(spec.child, ctx));
    return el;
  }
  if(kind==="component"){
    // A focus/event scope — purely behavioural in the TUI; renders its
    // child transparently here.
    if(spec.child) return widgetEl(spec.child, ctx);
    return div("w-component-empty");
  }
  if(kind==="raw"&&spec.entries){
    const text=spec.entries.map(entryText).join("\n");
    if(ctx.table){
      // A table cell: a rule, a caption, or air.
      const t=text.trim();
      if(/^[│┃|]$/.test(t)) return div("w-vrule");
      const c=div(t&&t===t.toUpperCase()&&/[A-Z]/.test(t)?"w-th":"w-td"); c.textContent=t; return c;
    }
    // Blank rows a terminal layout stacks to push what follows to the bottom
    // of the panel: a flexible gap here, which does the same at any height.
    if(!ctx.grid&&!text.trim()){
      // In a row, spaces are a gap as wide as they are; in a column, blank
      // rows push what follows down.
      if(ctx.inRow){ const g=div("w-gap"); g.style.width=Math.max(...spec.entries.map(e=>entryText(e).length),1)*0.6+"ch"; return g; }
      // A page (a panel mounted into a pane, scrolled as a whole) has no
      // bottom to push toward: its blank rows are the air between its blocks,
      // one row each, as the terminal sets them.
      if(ctx.slot==="pane"){ const b=div("w-blank"); b.style.height=CH*spec.entries.length+"px"; return b; }
      return div("w-fill");
    }
    const el=div("w-raw");
    spec.entries.forEach((e,i)=>{ if(i) el.append("\n"); fillEntry(el.appendChild(document.createElement("span")), e); });
    return el;
  }
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

// A tree row press by identity: the tree's key, the event, the row.
function routeTree(ctx, node, eventType, payload){
  if(ctx.kind!=="panel") return;
  const {col,row,...rest}=payload;
  const msg={...panelBase(ctx),widgetKey:node.key||"",eventType,payload:rest};
  if(col!=null){ msg.col=col; msg.row=row; }
  sendWidget(msg);
}

// Native Settings — a control's value rendered semantically (read from the
// shared model; keyboard drives edits via handle_key).
const SET_LIST_KINDS = new Set(["textList","map","objectArray","dualList"]);
// Inline control (toggle/number/dropdown/text/json/complex). `idx` = item index;
// `live` enables click routing (false inside the entry dialog, which is keyboard).
function settingControlEl(c, idx, live){
  const k=c.kind, el=document.createElement("span"); el.className="set-ctl set-"+k;
  if(k==="toggle"){ const sw=document.createElement("span"); sw.className="set-switch"+(c.checked?" on":""); aria(sw,"switch",{checked:!!c.checked}); el.appendChild(sw); if(live) sw.onmousedown=setHit("controlToggle",idx); }
  else if(k==="number"){
    const mk=(t,cls,hit)=>{const b=document.createElement("span");b.className=cls;b.textContent=t;if(live&&hit)b.onmousedown=setHit(hit,idx);return b;};
    // The value as the JSON carries it, shown the way the TUI's cell shows it:
    // a percent ×100, an integer whole.
    const disp=c.percent?Math.round(c.value*100)+"%":c.integer?String(Math.round(c.value)):String(+(+c.value).toFixed(3));
    el.appendChild(mk("−","set-step","controlDecrement")); const v=document.createElement("span"); v.className="set-num-v"; v.textContent=disp; el.appendChild(v); el.appendChild(mk("+","set-step","controlIncrement"));
  }
  else if(k==="dropdown"){ const p=document.createElement("span"); p.className="set-pill"; p.textContent=(c.options[c.selected]||"—")+" ▾"; if(live) p.onmousedown=setHit("controlDropdown",idx); el.appendChild(p);
    if(c.open){ const d=div("set-dd"); c.options.forEach((o,i)=>{const r=div("set-dd-row"+(i===c.selected?" sel":""));r.textContent=o;if(live)r.onmousedown=setHit("controlDropdownOption",idx,i);d.appendChild(r);});
      // Portal to the body-level host (same as the plugin Dropdown): the
      // Settings dialog is a transformed, overflow-clipping box with a
      // scrolling list, so an in-place list is cut off at the dialog edge (a
      // long option set — e.g. Default Language — is far wider than the compact
      // pill). Out of the modal it grows to fit the widest option and shifts to
      // stay in view (right-aligning under the flush-right control).
      mountPopover(d);
      requestAnimationFrame(()=>positionFloatingPopover(p, d, {align:"end"})); } }
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
    const row=aria(div("set-item"+(it.selected?" sel":"")+(it.readOnly?" ro":"")+(isList?" set-item-block":"")),
      "group", {label:it.name, current:it.selected?"true":null, readonly:it.readOnly||null});
    const head=div("set-item-head");
    const nm=div("set-name"); nm.textContent=it.name+(it.modified?" •":""); head.appendChild(nm);
    const selectHit = entry
      ? (it.readOnly ? null : e=>{ e.preventDefault(); e.stopPropagation(); sendSettings({kind:"entryItem", a:it.index}); })
      : (live ? setHit("item",it.index) : null);  // click the row label = select item
    if(selectHit){ nm.onmousedown=selectHit; }
    if(!isList){ const ctl=div("set-ctl-wrap"); ctl.appendChild(settingControlEl(it.control,it.index,live&&!entry));
      for(const named of ctl.querySelectorAll("[role]")) named.setAttribute("aria-label", it.name); head.appendChild(ctl); if(entry&&selectHit) ctl.onmousedown=selectHit; }
    row.appendChild(head);
    if(it.description){ const d=div("set-desc"); d.textContent=it.description; row.appendChild(d); }
    if(isList) row.appendChild(settingListEl(it.control,it.index,live&&!entry));
    container.appendChild(row);
  }
}
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
    else if(c.expandable){ const chev=document.createElement("span"); chev.className="set-cat-chev"; chev.textContent=c.expanded?"▼ ":"▶ "; chev.onmousedown=setHit("categoryDisclosure",c.index); r.appendChild(chev); } else r.appendChild(document.createTextNode("  "));
    const nm=document.createElement("span"); nm.textContent=c.name; r.appendChild(nm);
    r.onmousedown=setHit("category",c.index); cats.appendChild(r);
    if(c.expanded){ c.sections.forEach((sec,si)=>{ const sr=aria(div("set-cat-sec"),"treeitem",{selected:false,level:2,label:sec}); sr.textContent="   "+sec; sr.onmousedown=setHit("categorySection",c.index,si); cats.appendChild(sr); }); } }
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
    const bb=aria(div("btn"+(b==="Save"?" primary":"")+(sel?" sel":"")),"button"); bb.textContent=b; bb.onmousedown=setHit(fkinds[i]||"cancel"); ft.appendChild(bb);
  }); el.appendChild(ft);
  // entry dialog (Map/ObjectArray add/edit): rows are clickable (select+edit),
  // controls keyboard-driven, buttons routed by name to the entry dispatch.
  if(s.entryDialog){ const d=s.entryDialog; const dlg=aria(div("set-overlay set-entry"),"dialog",{modal:true,label:d.title});
    const dt=div("set-entry-title"); dt.textContent=d.title; dlg.appendChild(dt);
    const di=div("set-entry-items"); settingItemRows(d.items.map((it,i)=>({...it,selected:i===d.selectedItem&&!d.focusOnButtons})), di, true, true); dlg.appendChild(di);
    const db=div("set-dlg-btns");
    // Button focus index follows the TUI order [Save, Cancel, Delete]; the
    // visual order here puts Delete in the middle but each routes by name.
    const tuiIdx={Save:0,Cancel:1,Delete:2};
    const btns=d.noDelete?["Save","Cancel"]:["Save","Delete","Cancel"];
    btns.forEach(b=>{const f=d.focusOnButtons&&tuiIdx[b]===d.focusedButton; const bb=aria(div("btn"+(b==="Save"?" primary":b==="Delete"?" danger":"")+(f?" sel":"")),"button"); bb.textContent=b;
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
    row.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendClick({button:"left",col:cell.col,row:cell.row}); };
    row.onmouseenter=()=>sendMouse({kind:"moved",col:cell.col,row:cell.row});
    el.appendChild(row);
  });
  return el;
}

