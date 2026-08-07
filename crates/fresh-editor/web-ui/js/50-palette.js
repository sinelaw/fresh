// Native command palette / picker / prompt + file-browser band.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// ---- native command palette / picker ------------------------------------
// The editor owns the query, the filtered suggestions, the selection and the
// scroll window; we render them natively and forward row clicks / wheel back
// through handle_mouse at the pipeline's list cell rect so it stays in sync.
// Build the scrollable results list (rows map 1:1 to the editor's visible
// suggestion window; clicks/wheel route back through handle_mouse at the list
// cell rect so the editor stays the source of truth).
function paletteListEl(p, list){
  const listBox=div("plist");
  const start=p.scrollStart||0, n=p.visibleCount||p.suggestions.length;
  for(let j=0;j<n;j++){
    const idx=start+j; const s=p.suggestions[idx]; if(!s) break;
    const row=div("prow"+(idx===p.selected?" sel":"")+(s.disabled?" disabled":""));
    const tx=document.createElement("span"); tx.className="ptext"; tx.textContent=s.text; row.appendChild(tx);
    const ds=document.createElement("span"); ds.className="pdesc"; ds.textContent=s.description||""; row.appendChild(ds);
    if(s.keybinding){ const k=document.createElement("span"); k.className="pkey"; k.textContent=s.keybinding; row.appendChild(k); }
    if(list){ const cell={col:list.x+1,row:list.y+j};
      row.onmousedown=e=>{ if(s.disabled) return; e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col:cell.col,row:cell.row}); }; }
    listBox.appendChild(row);
  }
  if(list){ listBox.addEventListener("wheel",e=>{ e.stopPropagation();
    sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:list.x+1,row:list.y,n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true}); }
  return listBox;
}

// Native search-option toggles (Case / Whole Word / Regex / Confirm-each) for
// search/replace prompts — the TUI's checkbox row above the prompt line.
// Each chip forwards its click to the TUI's own checkbox cells
// (SearchOptionsLayout::checkbox_at), so the editor flips the flag and the
// refreshed state comes back through the scene — single source of truth.
function searchOptionsEl(p){
  const so=p.searchOptions;
  if(!so || !so.options || !so.options.length) return null;
  const bar=div("psearchopts");
  for(const o of so.options){
    const chip=div("psopt"+(o.active?" on":""));
    const box=document.createElement("span"); box.className="psbox";
    box.textContent=o.active?"✓":""; chip.appendChild(box);
    const lb=document.createElement("span"); lb.textContent=o.label; chip.appendChild(lb);
    if(o.shortcut){ const k=document.createElement("span"); k.className="pskey"; k.textContent=o.shortcut; chip.appendChild(k); }
    const col=o.x+Math.floor(o.w/2), row=so.row;
    chip.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendMouse({kind:"down",button:"left",col,row}); };
    bar.appendChild(chip);
  }
  return bar;
}

// ---- native file browser (OpenFile / SaveFileAs / SwitchProject) ---------
// The editor owns the directory, the entry window, the selection, the sort and
// the toggles; we render them natively from `palette.browser`
// (`Editor::file_browser_view`) and forward every click back through
// handle_mouse at the CELL SPAN the TUI laid that element out at — which the
// editor's own file-browser hit-tests resolve. Nothing here re-derives a
// position: the spans come from the renderer.
//
// The row window is deliberately the editor's own (`scrollOffset` …
// `+ visibleRows`): rendering more rows than the editor believes are visible
// would desync every click from `click_to_index`.
function fbClick(col,row,dbl){
  return e=>{ e.preventDefault(); e.stopPropagation();
    sendMouse({kind:"down",button:"left",col,row,count:dbl?e.detail:1}); };
}
// Click target for a recorded span: its middle cell, so a padded native
// control can never round outside the span the editor hit-tests.
function spanCell(s){ return {col:s.x+Math.floor((s.w||1)/2), row:s.y}; }

function fileBrowserEl(b,p){
  const el=div("palette centered filebrowser");
  // Title bar: the directory being browsed + the same close affordance the
  // command palette has.
  const tb=div("ptitlebar");
  // Path: the leading directories shrink with an ellipsis, the last segment
  // never does — so a deep path still tells you where you are.
  const tt=div("ptbtext fbpath"); tt.title=b.path;
  const cut=b.path.lastIndexOf("/");
  const phead=document.createElement("span"); phead.className="fbphead";
  phead.textContent=cut>0?b.path.slice(0,cut):"";
  const ptail=document.createElement("span"); ptail.className="fbptail";
  ptail.textContent=cut>=0?b.path.slice(cut):b.path;
  tt.appendChild(phead); tt.appendChild(ptail);
  tb.appendChild(tt);
  const xb=document.createElement("span"); xb.className="ptbclose"; xb.textContent="✕"; xb.title="Close (Esc)";
  xb.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendKey({key:"Escape"}); };
  tb.appendChild(xb); el.appendChild(tb);

  // The prompt input — inside the card, where the TUI's prompt row logically
  // belongs to this dialog even though the grid puts it on the last row.
  const bar=div("pinput");
  if(p.message){ const m=document.createElement("span"); m.className="pmsg"; m.textContent=p.message; bar.appendChild(m); }
  const q=document.createElement("span"); q.className="q";
  q.innerHTML=esc(p.query||"")+'<span class="caret2">&nbsp;</span>';
  bar.appendChild(q); scrollToCaret(q);
  if(p.status){ const st=document.createElement("span"); st.className="status"; st.textContent=p.status; bar.appendChild(st); }
  el.appendChild(bar);

  // Toolbar: navigation shortcuts on the left, checkbox toggles on the right.
  const tools=div("fbtools");
  const nav=div("fbnav");
  for(const s of b.shortcuts){
    const c=div("fbchip"+(s.selected?" sel":""));
    c.textContent=s.label; if(s.description) c.title=s.description;
    const cell=spanCell(s); c.onmousedown=fbClick(cell.col,cell.row);
    nav.appendChild(c);
  }
  tools.appendChild(nav);
  const tog=div("fbtoggles");
  for(const t of b.toggles){
    const c=div("fbtoggle"+(t.active?" on":""));
    const box=document.createElement("span"); box.className="fbbox"; box.textContent=t.active?"✓":""; c.appendChild(box);
    const lb=document.createElement("span"); lb.textContent=t.label; c.appendChild(lb);
    if(t.shortcut){ const k=document.createElement("span"); k.className="fbkey"; k.textContent=t.shortcut; c.appendChild(k); }
    const cell=spanCell(t); c.onmousedown=fbClick(cell.col,cell.row);
    tog.appendChild(c);
  }
  tools.appendChild(tog);
  el.appendChild(tools);

  // Column headers — clicking one re-sorts through the editor.
  const head=div("fbhead");
  for(const c of b.columns){
    const h=div("fbcol fbcol-"+c.name+(c.active?" sort":""));
    h.textContent=c.label+(c.active?(c.ascending?" ▲":" ▼"):"");
    const cell=spanCell(c); h.onmousedown=fbClick(cell.col,cell.row);
    head.appendChild(h);
  }
  el.appendChild(head);

  // Rows. A double-click opens (or descends into) the entry, exactly like the
  // TUI: the browser's click count rides along and the editor's own
  // double-click path decides what that means.
  const list=div("fbrows");
  if(b.loading){ const l=div("fbempty"); l.textContent="Loading…"; list.appendChild(l); }
  else if(b.error){ const l=div("fbempty fberr"); l.textContent=b.error; list.appendChild(l); }
  else if(!b.rows.length){ const l=div("fbempty"); l.textContent="(empty)"; list.appendChild(l); }
  for(const r of b.rows){
    const row=div("fbrow"+(r.selected?" sel":"")+(r.isDir?" dir":"")+(r.matchesFilter?"":" nomatch"));
    const nm=document.createElement("span"); nm.className="fbname";
    nm.textContent=r.name+(r.isDir?"/":(r.isSymlink?"@":"")); nm.title=r.name;
    const sz=document.createElement("span"); sz.className="fbsize"; sz.textContent=r.size||"—";
    const md=document.createElement("span"); md.className="fbmod"; md.textContent=r.modified||"—";
    row.appendChild(nm); row.appendChild(sz); row.appendChild(md);
    row.onmousedown=fbClick(b.listRect.x+1,r.row,true);
    list.appendChild(row);
  }
  // Wheel scrolls through the editor's own scroll offset, like the palette list.
  list.addEventListener("wheel",e=>{ e.stopPropagation();
    sendMouse({kind:e.deltaY>0?"scrolldown":"scrollup",col:b.listRect.x+1,row:b.listRect.y,
      n:Math.min(5,Math.max(1,Math.round(Math.abs(e.deltaY)/40)))}); },{passive:true});
  el.appendChild(list);

  const foot=div("fbfoot");
  const cnt=document.createElement("span");
  cnt.textContent=b.selected!=null?((b.selected+1)+" / "+b.total):(b.total+" items");
  foot.appendChild(cnt);
  el.appendChild(foot);
  return el;
}

// Input-only prompt (OpenFile / SaveFileAs / SwitchProject / plain inputs).
// With a file browser attached the whole dialog is one native card (the prompt
// input lives inside it); without one it stays the bottom prompt-row bar.
function paletteInputOnlyEls(p){
  if(p.browser){
    const scrim=div("modal-scrim");
    scrim.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendKey({key:"Escape"}); };
    return [scrim, fileBrowserEl(p.browser,p)];
  }
  return [paletteInputOnlyEl(p)];
}
function paletteInputOnlyEl(p){
  const el=div("palette input-only");
  // The prompt row replaces the status bar on the grid's last row, spanning
  // the chrome area right of the dock — never the dock columns or the
  // wallpaper outside the bezel.
  const dockPx=dockWidthPx();
  el.style.left=dockPx+"px";
  el.style.width=Math.max(0,px((scene&&scene.w)||0,CW)-dockPx)+"px";
  el.style.top="auto";
  const gridBottom=px((scene&&scene.h)||0,CH);
  el.style.bottom=Math.max(0, appH()-gridBottom)+"px";
  const bar=div("pinput");
  bar.style.borderBottom="none";
  bar.style.borderTop="1px solid var(--hairline)";
  // The TUI draws this prompt flush from column 0 across the full row; drop the
  // horizontal inset so the query box is as wide as the row the TUI fit its text
  // into (the inset was stealing ~3 cells and forcing needless truncation).
  bar.style.padding="7px 0";
  if(p.message){ const m=document.createElement("span"); m.className="pmsg"; m.textContent=p.message; bar.appendChild(m); }
  const q=document.createElement("span"); q.className="q";
  q.innerHTML=esc(p.query||"")+'<span class="caret2">&nbsp;</span>';
  bar.appendChild(q); scrollToCaret(q);
  if(p.status){ const st=document.createElement("span"); st.className="status"; st.textContent=p.status; bar.appendChild(st); }
  const so=searchOptionsEl(p); if(so) el.appendChild(so);   // TUI order: options above the prompt line
  el.appendChild(bar);
  return el;
}

// Keep the caret end of an overflowing input line in view (the TUI scrolls its
// prompt to the caret rather than eliding the tail). overflow:hidden boxes are
// still programmatically scrollable; measure after layout via rAF.
function scrollToCaret(q){ requestAnimationFrame(()=>{ q.scrollLeft = q.scrollWidth; }); }

// Build the palette CARD (title / toolbar / input / list / preview). Placement
// is decided from `paletteCentered`: a centered modal (CSS-positioned, input on
// top) or the terminal-style bottom sheet (input under the list, hugging the
// grid bottom). Either way the interior — and every row click / wheel route
// back to the editor's logical suggestion cell rect — is identical; only the
// container's geometry and the header/list order differ.
function paletteCardEl(p, list){
  const centered = paletteCentered;
  // When the overlay has a preview pane, the card spans BOTH columns and lays
  // out exactly like the TUI's single box: a full-width header (title/toolbar/
  // input) on top, then a body split into results | preview below. Otherwise
  // it's the compact VSCode-style command palette over the list rect.
  const hasPreview = !!(p.previewRect && p.previewCells && list);
  let outer = p.outerRect || list;
  if(hasPreview){ outer = {x:list.x, y:list.y, w:(p.previewRect.x+p.previewRect.w)-list.x, h:list.h}; }
  const el=div("palette"+(hasPreview?" with-preview":"")+(centered?" centered":""));
  // Position the card on the outer rect, with the input bar stacked above it so
  // it reads as one palette. (Clicks use the list cell rect below.)
  if(centered){
    // Centered modal: geometry is CSS-driven (transform-centered, capped by
    // max-width/height). A preview overlay still sizes its card to the editor
    // cell geometry so the results column lines up with the framed preview.
    if(hasPreview){ el.style.width=Math.max(360,px(outer.w,CW))+"px"; }
  } else if(hasPreview){
    // Preview overlays (quick-open / live-grep) keep the editor's geometry so the
    // results column lines up with the framed preview cells beside it.
    const inputH=34, w=Math.max(360,px(outer.w,CW));
    el.style.left=px(outer.x,CW)+"px";
    el.style.top=Math.max(4, px(outer.y,CH)-inputH)+"px";
    el.style.width=w+"px";
  } else {
    // TUI parity: the plain palette is a bottom sheet on the editor's own
    // geometry — the suggestion list sits where the pipeline's suggestions
    // box is, with the input bar UNDER it on the prompt row, hugging the
    // bottom of the cell grid exactly like the terminal. Row clicks still
    // route through the editor's list cell rect; only the pixels are ours.
    // The sheet spans the FULL grid width (from column 0, not outer.x), so it
    // also covers the file-explorer band the TUI overlays at these rows.
    el.style.left="0px";
    el.style.width=px((scene&&scene.w)||(outer.x+outer.w),CW)+"px";
    el.style.top="auto";
    const gridBottom=px((scene&&scene.h)||(outer.y+outer.h+1),CH);
    el.style.bottom=Math.max(0, appH()-gridBottom)+"px";
  }

  if(centered){
    // Centered modal gets a proper title bar with a close 'x' (Escape route),
    // in place of the plain title strip used by the bottom sheet.
    const tb=div("ptitlebar");
    const tt=document.createElement("span"); tt.className="ptbtext";
    tt.textContent=p.title||"Command Palette"; tb.appendChild(tt);
    const xb=document.createElement("span"); xb.className="ptbclose";
    xb.textContent="✕"; xb.title="Close (Esc)";
    xb.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendKey({key:"Escape"}); };
    tb.appendChild(xb);
    el.appendChild(tb);
  } else if(p.title){ const t=div("ptitle"); t.textContent=p.title; el.appendChild(t); }
  // plugin-built toolbar (real WidgetSpec widgets, e.g. live-grep scope toggles)
  if(p.toolbar){ const tb=div("ptoolbar"); tb.appendChild(widgetEl(p.toolbar, {kind:"toolbar", focusKey:p.toolbarFocus})); el.appendChild(tb); }
  // input bar (query + status + count)
  const bar=div("pinput");
  const q=document.createElement("span"); q.className="q";
  q.innerHTML=esc(p.query||"")+'<span class="caret2">&nbsp;</span>';
  bar.appendChild(q); scrollToCaret(q);
  if(p.status){ const st=document.createElement("span"); st.className="status"; st.textContent=p.status; bar.appendChild(st); }
  const cnt=document.createElement("span"); cnt.className="count";
  cnt.textContent=(p.total!=null)?((p.selected!=null?p.selected+1:0)+" / "+p.total):"";
  bar.appendChild(cnt);

  const listBox=paletteListEl(p, list);
  if(hasPreview){
    // Overlay layout keeps the TUI's single-box order: header (input) on top,
    // then the results | preview body.
    el.appendChild(bar);
    const body=div("pbody");
    listBox.style.flex="0 0 "+px(list.w,CW)+"px";
    const h=px(p.previewRect.h,CH)+"px"; listBox.style.maxHeight=h;
    const pv=div("ppreview"); pv.style.flex="1"; pv.style.height=h;
    pv.innerHTML=cellsSvg(p.previewCells, p.previewRect.w);
    body.appendChild(listBox); body.appendChild(pv);
    el.appendChild(body);
  } else if(centered){
    // Centered modal: header on top (input → options → results) — the familiar
    // command-palette order.
    el.appendChild(bar);
    const so=searchOptionsEl(p); if(so) el.appendChild(so);
    el.appendChild(listBox);
  } else {
    // Bottom sheet: list above, input UNDER it — the terminal's prompt order,
    // with the search-options row (when present) between them like the TUI.
    el.appendChild(listBox);
    const so=searchOptionsEl(p); if(so) el.appendChild(so);
    bar.style.borderBottom="none";
    bar.style.borderTop="1px solid var(--hairline)";
    el.appendChild(bar);
  }
  return el;
}

// Palette region nodes: the card, plus a dimming `.modal-scrim` behind it in
// centered-modal mode (same pattern as settings / aux modals). Input-only
// prompts (OpenFile / SaveFileAs) have NO native suggestion list — their file
// browser is drawn into the pane cells already — so they always keep their
// bottom prompt-row input bar (a floating modal would detach it from its list)
// and never take a scrim.
function paletteEls(p){
  const list = p.listRect || p.outerRect;
  if(!list) return paletteInputOnlyEls(p);
  const card = paletteCardEl(p, list);
  if(!paletteCentered) return [card];
  const scrim = div("modal-scrim");
  // Click-away dismiss: a command palette is lightweight, so tapping the
  // backdrop closes it (Escape through the real editor). stopPropagation keeps
  // the click from also reaching the buffer's mouse handler underneath.
  scrim.onmousedown=e=>{ e.preventDefault(); e.stopPropagation(); sendKey({key:"Escape"}); };
  return [scrim, card];
}

