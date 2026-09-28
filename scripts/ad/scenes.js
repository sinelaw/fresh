// The fast cut. Times inside draw() are scene-local seconds.
// Bars are at 137 BPM (1 bar = 1.752 s); every cut lands on a downbeat.
//
// Every clip is recorded at the same 140x75 size. `z` is the zoom over the
// whole screen fitted into the window; shots anchor to the left edge (fx 0)
// so the gutter is never cropped.

const B = 60 / 137;           // one beat

const FEATURES = [
  'Command palette', 'Multi-cursor', 'Live grep', 'Themes', 'LSP', 'Autocomplete',
  'Go to definition', 'Rename symbol', 'Git gutter', 'Review diff', 'Git log', 'Git blame',
  'Split panes', 'Integrated terminal', 'File explorer', 'Keyboard macros', 'Vim mode',
  'Emacs keys', 'SSH remote editing', 'Multi-GB files', 'Hot exit', 'Daemon mode',
  'Markdown compose', 'Code tours', 'TypeScript plugins', 'Settings UI', 'Keybinding editor',
  'Mouse support', 'Orchestrator', 'Git worktrees', 'Coding agents', 'Search & replace',
  'Bookmarks', 'Diagnostics panel', 'Rainbow brackets', 'Dev containers', 'Sudo save',
  'Flash jump', 'Package manager', 'Encoding detection',
];

// ---- shared scene pieces ----------------------------------------------------
// camera keyframes: [[sceneT, {z, fx, fy}], ...], eased between keys over `ease` s
function camPath(keys, lt, ease = 0.35) {
  let cur = keys[0][1];
  for (let i = 1; i < keys.length; i++) {
    const [kt, k] = keys[i];
    if (lt < kt) break;
    const f = easeInOut((lt - kt) / ease);
    cur = { z: lerp(cur.z, k.z, f), fx: lerp(cur.fx, k.fx, f), fy: lerp(cur.fy, k.fy, f) };
  }
  return cur;
}

function termScene(o) {
  return (lt, dur, t) => {
    const clip = clips[o.clip];
    const r = pmap(o.map, lt);
    const punch = 0.035 * Math.exp(-lt * 14);
    const cam = typeof o.cam === 'function' ? o.cam(lt, dur, r) : Array.isArray(o.cam) ? camPath(o.cam, lt) : o.cam;
    brandBug(1);
    drawWindow({ clip, r, title: typeof o.title === 'function' ? o.title(lt, r) : o.title, cam, punch,
      pointer: o.pointer, scale: clipScale(clip) });
    for (const [a, b, text, co] of o.captions) {
      if (lt >= a && lt < b) caption(text, lt - a, b - a, Object.assign({ y: 330 }, co || {}));
    }
    if (o.chips) drawKeyChips(clip, o.map, lt, o.chips, o.chipLabels);
    if (o.overlay) o.overlay(lt, dur, r);
  };
}

function clipScale(clip) {
  return Math.min(TW / (clip.cols * CW), TH / (clip.rows * CHH));
}

// key chips from the recording's key events, placed at their mapped scene time
function drawKeyChips(clip, map, lt, allow, labels = {}) {
  const evs = [];
  for (const [rt, k] of clip.keys) {
    if (!allow.includes(k)) continue;
    const st = pinv(map, rt);
    if (st !== null && st <= lt + 1e-6) evs.push([st, labels[k] || k]);
  }
  if (!evs.length) return;
  const [st, label] = evs[evs.length - 1];
  let n = 0;
  for (let i = evs.length - 1; i >= 0 && evs[i][1] === label; i--) n++;
  chip(label + (n > 1 ? `  ×${n}` : ''), W / 2, 1745, clamp((lt - st) / 0.18));
}

function slam(text, lt, x, y, size, color, rot, o = {}) {
  if (lt < 0) return;
  const f = clamp(lt / 0.09);
  const s = lerp(2.4, 1, easeOut(f));
  ctx.save();
  ctx.translate(x, y); ctx.rotate(rot); ctx.scale(s, s);
  ctx.globalAlpha = (o.alpha ?? 1) * clamp(0.5 + f * 2);
  ctx.font = `${o.weight || 800} ${size}px ${o.font || 'JBM'}`;
  ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
  if (o.glitch) {
    ctx.fillStyle = 'rgba(0,255,255,0.7)'; ctx.fillText(text, -6, 0);
    ctx.fillStyle = 'rgba(255,0,80,0.7)'; ctx.fillText(text, 6, 0);
  }
  ctx.shadowColor = color; ctx.shadowBlur = o.glow ?? 40;
  ctx.fillStyle = color; ctx.fillText(text, 0, 0);
  ctx.restore();
}

function shake(amount, t) {
  if (amount <= 0) return;
  ctx.translate((hash(Math.floor(t * 60)) - .5) * amount, (hash(Math.floor(t * 60) + 7) - .5) * amount);
}

// one row of feature pills scrolling sideways, wrapping around
function featureRow(t, y, speed, offset, o = {}) {
  const size = o.size || 38, gap = 22;
  const colors = [C.green, C.blue, C.purple, C.yellow];
  ctx.save();
  ctx.font = `800 ${size}px JBM`;
  const items = FEATURES.slice(offset).concat(FEATURES.slice(0, offset));
  const widths = items.map(s => ctx.measureText(s).width + size * 1.1);
  const total = widths.reduce((a, w) => a + w + gap, 0);
  let x = -(((t * speed) % total) + total) % total;
  ctx.globalAlpha = o.alpha ?? 1;
  ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
  for (let rep = 0; rep < 3 && x < W; rep++) {
    items.forEach((s, i) => {
      const w = widths[i], h = size * 1.7;
      if (x + w > -50 && x < W + 50) {
        rrect(x, y - h / 2, w, h, h * 0.3);
        ctx.fillStyle = 'rgba(255,255,255,0.06)'; ctx.fill();
        ctx.lineWidth = 2.5; ctx.strokeStyle = colors[(i + offset) % 4]; ctx.stroke();
        ctx.fillStyle = '#fff'; ctx.fillText(s, x + w / 2, y + 2);
      }
      x += w + gap;
    });
  }
  ctx.restore();
}

// ---- the scenes ------------------------------------------------------------
const SCENES = [
  // 0 — HOOK: the thing everybody has typed at a terminal editor
  {
    start: 0, bars: 1, energy: 0, flash: false,
    draw(lt, dur, t) {
      ctx.fillStyle = '#000'; ctx.fillRect(0, 0, W, H);
      const beat = Math.floor(lt / B), bl = lt - beat * B;
      ctx.save();
      shake(beat < 3 ? 34 * Math.exp(-bl * 14) : 10 + 30 * (bl / B), lt);
      ctx.save();
      ctx.globalAlpha = easeOut(lt / 0.15);
      rrect(90, 250, W - 180, 120, 60); ctx.fillStyle = '#16181d'; ctx.fill();
      ctx.strokeStyle = '#2a2e36'; ctx.lineWidth = 3; ctx.stroke();
      ctx.strokeStyle = C.muted; ctx.lineWidth = 6; ctx.beginPath(); ctx.arc(165, 303, 20, 0, 7); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(180, 318); ctx.lineTo(198, 336); ctx.stroke();
      const q = 'how to exit terminal editor';
      const n = Math.floor(clamp(lt / 0.75) * q.length);
      ctx.font = '600 44px JBM'; ctx.fillStyle = C.text; ctx.textBaseline = 'middle';
      ctx.fillText(q.slice(0, n) + (Math.floor(lt * 6) % 2 ? '▏' : ''), 225, 311);
      ctx.restore();
      slam(':q!', lt, 540, 760, 250, C.red, -0.07, { glitch: beat === 0, alpha: beat > 0 ? 0.35 : 1 });
      slam(':wq!!', lt - B, 560, 1070, 210, C.yellow, 0.06, { glitch: beat === 1, alpha: beat > 1 ? 0.35 : 1 });
      slam('^X ^C', lt - 2 * B, 520, 1360, 200, C.purple, -0.04, { glitch: beat === 2, alpha: beat > 2 ? 0.35 : 1 });
      if (beat >= 3) slam('HELP', lt - 3 * B, 540, 1060, 330, '#ffffff', 0.02, { glitch: true, font: 'Inter', weight: 900, glow: 80 });
      ctx.restore();
      if (lt > dur - 0.12) { ctx.fillStyle = `rgba(255,255,255,${(lt - (dur - 0.12)) / 0.12})`; ctx.fillRect(0, 0, W, H); }
    },
  },

  // 1 — LOGO DROP
  {
    start: 1, bars: 1, energy: 1, flash: false,
    draw(lt, dur, t) {
      const cx = W / 2, cy = 760;
      for (let i = 0; i < 3; i++) {
        const f = clamp((lt - i * 0.08) / 0.9);
        if (f <= 0 || f >= 1) continue;
        ctx.strokeStyle = `rgba(16,185,129,${0.7 * (1 - f)})`; ctx.lineWidth = 16 * (1 - f) + 2;
        ctx.beginPath(); ctx.arc(cx, cy, 120 + f * 900, 0, 7); ctx.stroke();
      }
      const glow = ctx.createRadialGradient(cx, cy, 0, cx, cy, 520);
      glow.addColorStop(0, 'rgba(16,185,129,0.45)'); glow.addColorStop(1, 'rgba(16,185,129,0)');
      ctx.fillStyle = glow; ctx.fillRect(0, 0, W, H);
      const size = 420 * backOut(lt / 0.35);
      ctx.save(); ctx.translate(cx, cy); ctx.rotate((1 - easeOut(lt / 0.5)) * -0.5);
      ctx.drawImage(imgs.logo, -size / 2, -size / 2, size, size); ctx.restore();
      if (lt > B) {
        const wf = backOut((lt - B) / 0.3);
        ctx.save(); ctx.globalAlpha = clamp((lt - B) / 0.1);
        ctx.translate(cx, 1150); ctx.scale(lerp(0.5, 1, wf), lerp(0.5, 1, wf));
        ctx.font = '900 210px Inter'; ctx.textAlign = 'center'; ctx.textBaseline = 'alphabetic';
        ctx.fillStyle = C.text; ctx.shadowColor = 'rgba(16,185,129,.6)'; ctx.shadowBlur = 50;
        ctx.fillText('Fresh', 0, 0); ctx.restore();
      }
      if (lt > 2 * B) caption('The *terminal* IDE.', lt - 2 * B, dur - 2 * B, { y: 1320, size: 72 });
      if (lt > 3 * B) caption('Zero learning curve.', lt - 3 * B, dur - 3 * B, { y: 1430, size: 60, color: C.muted, weight: 900 });
      if (lt < 0.15) { ctx.fillStyle = `rgba(255,255,255,${1 - lt / 0.15})`; ctx.fillRect(0, 0, W, H); }
    },
  },

  // 2-3 — launch from the shell, then the keys you already know
  {
    start: 2, bars: 2, clip: 'code',
    draw: termScene({
      clip: 'code',
      title: lt => lt < 0.8 ? 'bash' : 'fresh — main.rs',
      map: [[0, 1.3], [0.62, 2.6], [0.8, 3.35], [0.95, 3.5], [0.95, 8.85], [1.62, 10.3],
            [1.66, 11.62], [1.75, 12.34], [2.19, 13.54], [2.63, 14.94], [3.07, 16.64], [3.504, 17.6]],
      cam: [[0, { z: 3.0, fx: 0, fy: 0 }], [0.8, { z: 2.3, fx: 0, fy: 0 }], [1.2, { z: 2.5, fx: 0, fy: 0 }]],
      captions: [[0, 0.8, 'Lives in your *terminal.*'], [0.8, 3.504, 'Keys you *already* know.']],
      chips: ['Ctrl+C', 'Ctrl+V', 'Ctrl+Z', 'Ctrl+S'],
    }),
  },

  // 4 — mouse
  {
    start: 4, bars: 1, clip: 'mouse',
    draw: termScene({
      clip: 'mouse', title: 'fresh — server.rs', pointer: true,
      map: [[0, 4.2], [0.35, 4.95], [0.45, 5.1], [0.85, 7.4], [0.86, 8.9], [1.3, 10.05], [1.752, 12.6]],
      cam: { z: 2.2, fx: 0, fy: 0 },
      captions: [[0, 1.752, 'Mouse? *Obviously.*']],
    }),
  },

  // 5 — command palette: anchored bottom-left so the input line stays in frame
  {
    start: 5, bars: 1, clip: 'palette',
    draw: termScene({
      clip: 'palette', title: 'fresh — server.rs',
      map: [[0, 4.3], [0.2, 4.55], [0.44, 6.45], [0.88, 8.75], [1.31, 11.05], [1.5, 12.6], [1.752, 13.0]],
      cam: [[0, { z: 2.2, fx: 0, fy: 1 }], [1.5, { z: 2.2, fx: 0, fy: 0 }]],
      captions: [[0, 1.752, '*Ctrl+P* does\neverything.']],
      chips: ['Ctrl+P'],
    }),
  },

  // 6 — themes, one per 8th note
  {
    start: 6, bars: 1, clip: 'themes',
    draw: termScene({
      clip: 'themes', title: 'fresh — server.rs',
      map: [[0, 8.5], ...[0, 1, 2, 3, 4, 5, 6].map(k => [0.08 + 0.219 * k, 8.75 + 0.9 * k]), [1.752, 15.1]],
      cam: lt => ({ z: lerp(2.0, 2.15, lt / 1.75), fx: 0, fy: 0 }),
      captions: [[0, 1.752, 'Themes. *Live.*']],
    }),
  },

  // 7 — multi-cursor
  {
    start: 7, bars: 1, clip: 'multicursor',
    draw: termScene({
      clip: 'multicursor', title: 'fresh — server.rs',
      map: [[0, 8.55], ...[0, 1, 2, 3, 4, 5].map(k => [0.05 + 0.2 * k, 8.8 + 0.9 * k]), [1.2, 14.35], [1.62, 16.9], [1.752, 17.0]],
      cam: { z: 2.7, fx: 0, fy: 0.08 },
      captions: [[0, 1.752, 'Edit it *all*\nat once.']],
      chips: ['Ctrl+D'],
    }),
  },

  // 8 — live grep
  {
    start: 8, bars: 1, clip: 'grep',
    draw: termScene({
      clip: 'grep', title: 'fresh — server.rs',
      map: [[0, 6.0], [0.15, 6.3], [0.45, 7.8], [0.7, 8.65], [0.85, 9.5], [1.05, 11.95], [1.25, 12.95], [1.45, 13.85], [1.752, 14.9]],
      cam: { z: 1.9, fx: 0, fy: 0 },
      captions: [[0, 1.752, 'Grep the *whole*\nproject.']],
    }),
  },

  // 9 — huge file
  {
    start: 9, bars: 1, clip: 'huge',
    draw: termScene({
      clip: 'huge', title: lt => lt < 0.95 ? 'bash' : 'fresh — huge.log',
      map: [[0, 1.4], [0.28, 2.85], [0.5, 4.0], [0.78, 4.9], [0.92, 5.65], [1.2, 5.9], [1.21, 9.7], [1.3, 9.85], [1.752, 10.6]],
      cam: [[0, { z: 3.0, fx: 0, fy: 0 }], [0.92, { z: 2.3, fx: 0, fy: 0 }]],
      captions: [[0, 0.92, '*2 GB* log file?'], [0.92, 1.752, '*No problem.*']],
      chips: ['Ctrl+End'],
    }),
  },

  // 10 — settings UI: no config files
  {
    start: 10, bars: 1, clip: 'settings',
    draw: termScene({
      clip: 'settings', title: 'fresh — settings',
      map: [[0, 6.9], [0.3, 7.2], [0.55, 11.35], [0.9, 13.65], [1.1, 14.5], [1.32, 15.97], [1.752, 16.8]],
      cam: { z: 1.8, fx: 0, fy: 0 },
      captions: [[0, 0.88, 'No config files.'], [0.88, 1.752, 'Just *settings.*']],
    }),
  },

  // 11-12 — the Orchestrator: three workspaces that look different. The recording
  // switches split -> agent (170.7) -> main checkout's Review Diff (174.6) -> agent
  // (178.7) -> split (182.6); each switch lands on a beat.
  {
    start: 11, bars: 2, clip: 'agents',
    draw: termScene({
      clip: 'agents', title: 'fresh — orchestrator',
      map: [[0, 170.1], [0.40, 170.75], [0.8, 171.4], [0.84, 174.65], [1.6, 175.5], [1.72, 178.7], [2.5, 179.6], [2.62, 182.65], [3.504, 184.5]],
      // agent / diff: the dock plus the top of the pane; split: pan right onto agent + file
      cam: [[0, { z: 1.2, fx: 1, fy: 0 }], [0.38, { z: 1.7, fx: 0, fy: 0 }],
            [2.6, { z: 1.0, fx: 0, fy: 0 }], [2.95, { z: 1.2, fx: 1, fy: 0 }]],
      captions: [[0, 1.752, 'One workspace\n*per task.*', { y: 360 }], [1.752, 3.504, 'Agents, diffs, code.\n*Side by side.*', { y: 360 }]],
      overlay(lt) {
        ctx.save();
        ctx.globalAlpha = easeOut(lt / 0.2);
        ctx.font = '800 40px JBM';
        const label = 'ORCHESTRATOR';
        const w = ctx.measureText(label).width + 50;
        rrect(W / 2 - w / 2, 190, w, 62, 31);
        ctx.fillStyle = 'rgba(16,185,129,0.16)'; ctx.fill();
        ctx.strokeStyle = C.green; ctx.lineWidth = 2.5; ctx.stroke();
        ctx.fillStyle = C.green2; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
        ctx.fillText(label, W / 2, 223);
        ctx.restore();
        ['claude', 'codex', 'opencode', 'aider'].forEach((a, i) => {
          const at = 1.752 + i * B / 2;
          if (lt > at) chip(a, 170 + i * 247, 1745, clamp((lt - at) / 0.18), { size: 34, border: [C.green, C.blue, C.purple, C.yellow][i] });
        });
      },
    }),
  },

  // 13 — and everything else: a wall of features flowing past
  {
    start: 13, bars: 1, clip: 'blitz',
    draw(lt, dur, t) {
      ctx.fillStyle = 'rgba(0,0,0,0.55)'; ctx.fillRect(0, 0, W, H);
      for (let i = 0; i < 9; i++) {
        const dir = i % 2 ? 1 : -1;
        const inF = easeOut((lt - i * 0.03) / 0.25);
        ctx.save(); ctx.translate(dir * (1 - inF) * 400, 0);
        featureRow(lt + 3, 520 + i * 150, dir * (650 + 90 * (i % 3)), (i * 5) % FEATURES.length, { alpha: inF });
        ctx.restore();
      }
      const band = ctx.createLinearGradient(0, 160, 0, 460);
      band.addColorStop(0, 'rgba(7,9,11,0.95)'); band.addColorStop(1, 'rgba(7,9,11,0)');
      ctx.fillStyle = band; ctx.fillRect(0, 0, W, 470);
      caption('And *so much* more.', lt, dur, { y: 330, size: 100 });
    },
  },

  // 14-16 — end card
  {
    start: 14, bars: 3.14, energy: 0.6,
    draw(lt, dur, t) {
      const cx = W / 2;
      const glow = ctx.createRadialGradient(cx, 640, 0, cx, 640, 600);
      glow.addColorStop(0, `rgba(16,185,129,${0.35 + 0.1 * Math.sin(lt * 3)})`); glow.addColorStop(1, 'rgba(16,185,129,0)');
      ctx.fillStyle = glow; ctx.fillRect(0, 0, W, H);
      const size = 340 * backOut(lt / 0.4);
      ctx.save(); ctx.translate(cx, 640 + Math.sin(lt * 2) * 8);
      ctx.drawImage(imgs.logo, -size / 2, -size / 2, size, size); ctx.restore();
      if (lt > 0.12) {
        const wf = backOut((lt - 0.12) / 0.3);
        ctx.save(); ctx.globalAlpha = clamp((lt - 0.12) / 0.1);
        ctx.translate(cx, 1000); ctx.scale(lerp(0.6, 1, wf), lerp(0.6, 1, wf));
        ctx.font = '900 200px Inter'; ctx.textAlign = 'center';
        ctx.fillStyle = C.text; ctx.shadowColor = 'rgba(16,185,129,.55)'; ctx.shadowBlur = 50;
        ctx.fillText('Fresh', 0, 0); ctx.restore();
      }
      if (lt > B) caption('The terminal IDE that *just works.*', lt - B, 99, { y: 1130, size: 50, exit: false, stagger: 0.03 });
      if (lt > 2 * B) {
        const f = backOut((lt - 2 * B) / 0.35);
        ctx.save(); ctx.translate(cx, 1330); ctx.scale(lerp(0.5, 1, f), lerp(0.5, 1, f));
        ctx.globalAlpha = clamp((lt - 2 * B) / 0.1);
        ctx.font = '800 76px JBM';
        const w = ctx.measureText('getfresh.dev').width + 110;
        ctx.shadowColor = C.green; ctx.shadowBlur = 30 + 30 * (0.5 + 0.5 * Math.sin(lt * 5));
        rrect(-w / 2, -70, w, 140, 70); ctx.fillStyle = C.green; ctx.fill(); ctx.shadowBlur = 0;
        ctx.fillStyle = '#04120c'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
        ctx.fillText('getfresh.dev', 0, 4);
        ctx.restore();
      }
      if (lt > 4 * B) caption('Free & open source', lt - 4 * B, 99, { y: 1520, size: 46, color: C.text, exit: false, weight: 800 });
      if (lt > 5 * B) caption('Linux · macOS · Windows', lt - 5 * B, 99, { y: 1595, size: 40, color: C.muted, exit: false, weight: 800 });
      // the feature list keeps flowing along the bottom
      featureRow(lt, 1760, -120, 3, { size: 30, alpha: 0.55 * easeOut(lt / 0.6) });
      featureRow(lt, 1850, 100, 21, { size: 30, alpha: 0.4 * easeOut(lt / 0.6) });
    },
  },
];
