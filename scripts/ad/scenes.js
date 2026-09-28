// Scene list for the Fresh ad. Times inside draw() are scene-local seconds.
// Bars are at 137 BPM (1 bar = 1.752 s); every cut lands on a downbeat.

const B = 60 / 137;           // one beat

// ---- shared scene pieces ----------------------------------------------------
function termScene(o) {
  // o: clip, map, title, caption(s), cam, chips, pointer, capY
  return (lt, dur, t) => {
    const clip = clips[o.clip];
    const r = pmap(o.map, lt);
    const punch = 0.035 * Math.exp(-lt * 14);
    const cam = typeof o.cam === 'function' ? o.cam(lt, dur) : (o.cam || { z: 1, fx: .5, fy: .5 });
    brandBug(1);
    drawWindow({ clip, r, title: typeof o.title === 'function' ? o.title(lt) : o.title, cam, punch, pointer: o.pointer,
      scale: clipScale(clip) });
    // captions: [[from, to, text, opts]]
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
  // show the latest key big, with a counter if the same key repeats
  const [st, label] = evs[evs.length - 1];
  let n = 0;
  for (let i = evs.length - 1; i >= 0 && evs[i][1] === label; i--) n++;
  const pop = clamp((lt - st) / 0.18);
  chip(label + (n > 1 ? `  ×${n}` : ''), W / 2, 1745, pop);
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
      // search-bar style question
      ctx.save();
      const qa = easeOut(lt / 0.15);
      ctx.globalAlpha = qa;
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
      if (beat >= 3) {
        slam('HELP', lt - 3 * B, 540, 1060, 330, '#ffffff', 0.02, { glitch: true, font: 'Inter', weight: 900, glow: 80 });
      }
      ctx.restore();
      // whiteout into the drop
      if (lt > dur - 0.12) { ctx.fillStyle = `rgba(255,255,255,${(lt - (dur - 0.12)) / 0.12})`; ctx.fillRect(0, 0, W, H); }
    },
  },

  // 1 — LOGO DROP
  {
    start: 1, bars: 1, energy: 1, flash: false,
    draw(lt, dur, t) {
      // white flash out of the drop
      const cx = W / 2, cy = 760;
      // shockwave rings
      for (let i = 0; i < 3; i++) {
        const f = clamp((lt - i * 0.08) / 0.9);
        if (f <= 0 || f >= 1) continue;
        ctx.strokeStyle = `rgba(16,185,129,${0.7 * (1 - f)})`; ctx.lineWidth = 16 * (1 - f) + 2;
        ctx.beginPath(); ctx.arc(cx, cy, 120 + f * 900, 0, 7); ctx.stroke();
      }
      const glow = ctx.createRadialGradient(cx, cy, 0, cx, cy, 520);
      glow.addColorStop(0, 'rgba(16,185,129,0.45)'); glow.addColorStop(1, 'rgba(16,185,129,0)');
      ctx.fillStyle = glow; ctx.fillRect(0, 0, W, H);
      const s = backOut(lt / 0.35);
      const size = 420 * s;
      ctx.save(); ctx.translate(cx, cy); ctx.rotate((1 - easeOut(lt / 0.5)) * -0.5);
      ctx.drawImage(imgs.logo, -size / 2, -size / 2, size, size); ctx.restore();
      // wordmark
      const wf = backOut((lt - B) / 0.3);
      if (lt > B) {
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
      map: [[0, 1.3], [0.62, 2.6], [0.8, 3.3], [0.95, 3.5], [0.95, 8.85], [1.62, 10.2],
            [1.66, 11.62], [1.75, 12.34], [2.19, 13.54], [2.63, 14.94], [3.07, 16.64], [3.504, 17.6]],
      cam: lt => lt < 0.8 ? { z: 1.55, fx: 0, fy: 0 } : { z: lerp(1.25, 1.4, easeInOut((lt - 0.8) / 2.7)), fx: 0.1, fy: 0.12 },
      captions: [
        [0, 0.8, 'Lives in your *terminal.*'],
        [0.8, 3.504, 'Keys you *already* know.'],
      ],
      chips: ['Ctrl+C', 'Ctrl+V', 'Ctrl+Z', 'Ctrl+S'],
    }),
  },

  // 4 — mouse
  {
    start: 4, bars: 1, clip: 'mouse',
    draw: termScene({
      clip: 'mouse', title: 'fresh — server.rs', pointer: true,
      map: [[0, 4.2], [0.35, 4.95], [0.45, 5.1], [0.85, 7.4], [0.86, 8.9], [1.3, 10.05], [1.75, 12.6]],
      cam: lt => ({ z: 1.3, fx: 0.05, fy: lt < 0.85 ? 0.05 : 0.35 }),
      captions: [[0, 1.752, 'Mouse? *Obviously.*']],
    }),
  },

  // 5 — command palette
  {
    start: 5, bars: 1, clip: 'palette',
    draw: termScene({
      clip: 'palette', title: 'fresh — server.rs',
      map: [[0, 4.3], [0.2, 4.5], [0.44, 6.45], [0.88, 8.75], [1.31, 11.05], [1.5, 12.6], [1.752, 13.0]],
      cam: lt => lt < 1.5 ? { z: 1.35, fx: 0.5, fy: 1 } : { z: 1.2, fx: 0.3, fy: 0.2 },
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
      cam: lt => ({ z: lerp(1.0, 1.12, lt / 1.75), fx: 0.3, fy: 0.3 }),
      captions: [[0, 1.752, 'Themes. *Live.*']],
    }),
  },

  // 7 — multi-cursor
  {
    start: 7, bars: 1, clip: 'multicursor',
    draw: termScene({
      clip: 'multicursor', title: 'fresh — server.rs',
      map: [[0, 8.55], ...[0, 1, 2, 3, 4, 5].map(k => [0.05 + 0.2 * k, 8.8 + 0.9 * k]), [1.2, 14.2], [1.62, 16.8], [1.752, 17.0]],
      cam: { z: 1.6, fx: 0.25, fy: 0.46 },
      captions: [[0, 1.752, 'Edit it *all*\nat once.']],
      chips: ['Ctrl+D'],
    }),
  },

  // 8 — live grep
  {
    start: 8, bars: 1, clip: 'grep',
    draw: termScene({
      clip: 'grep', title: 'fresh — server.rs',
      map: [[0, 5.9], [0.15, 6.2], [0.45, 7.8], [0.7, 8.65], [0.85, 9.5], [1.05, 11.95], [1.25, 12.95], [1.45, 13.85], [1.752, 14.9]],
      cam: { z: 1.15, fx: 0.4, fy: 0.3 },
      captions: [[0, 1.752, 'Grep the *whole*\nproject.']],
    }),
  },

  // 9 — huge file
  {
    start: 9, bars: 1, clip: 'huge',
    draw: termScene({
      clip: 'huge', title: lt => lt < 0.95 ? 'bash' : 'fresh — huge.log',
      map: [[0, 1.4], [0.28, 2.85], [0.5, 4.0], [0.78, 4.9], [0.92, 5.62], [1.2, 5.9], [1.21, 9.62], [1.3, 9.8], [1.752, 10.6]],
      cam: lt => lt < 0.92 ? { z: 1.6, fx: 0, fy: 0 } : { z: 1.3, fx: 0, fy: 0.1 },
      captions: [[0, 0.92, '*2 GB* log file?', {}], [0.92, 1.752, '*No problem.*']],
      chips: ['Ctrl+End'],
    }),
  },

  // 10 — AI agents in the Orchestrator
  {
    start: 10, bars: 1, clip: 'agents',
    draw: termScene({
      clip: 'agents', title: 'fresh — orchestrator',
      map: [[0, 96.6], [0.24, 97.14], [0.32, 97.6], [0.68, 99.58], [0.76, 100.1], [1.12, 102.08], [1.2, 102.6], [1.56, 104.64], [1.752, 105.1]],
      cam: { z: 1.12, fx: 0, fy: 0 },
      captions: [[0, 1.752, 'AI agents,\n*side by side.*', { y: 360 }]],
      overlay(lt) {
        // feature label above the caption
        ctx.save();
        const a = easeOut(lt / 0.2);
        ctx.globalAlpha = a;
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
          const at = 0.1 + i * B / 2;
          if (lt > at) chip(a, 170 + i * 247, 1745, clamp((lt - at) / 0.18), { size: 34, border: [C.green, C.blue, C.purple, C.yellow][i] });
        });
      },
    }),
  },

  // 11 — review diff
  {
    start: 11, bars: 1, clip: 'review',
    draw: termScene({
      clip: 'review', title: 'fresh — review diff',
      map: [[0, 6.4], [0.25, 6.7], [0.55, 11.1], [0.65, 11.35], [0.95, 12.05], [1.05, 12.3], [1.3, 14.15], [1.4, 14.4], [1.752, 15.2]],
      cam: { z: 1.25, fx: 0, fy: 0.2 },
      captions: [[0, 1.752, 'Git review,\n*built in.*']],
      chips: ['s'], chipLabels: { s: 's  stage hunk' },
    }),
  },

  // 12-13 — feature blitz
  {
    start: 12, bars: 2, clip: 'blitz',
    draw(lt, dur, t) {
      const beat = Math.floor(lt / B), bl = lt - beat * B;
      const shots = [
        ['blitz', 5.0, 'File explorer', { z: 1.25, fx: 0, fy: 0.1 }],
        ['blitz', 9.5, 'Settings UI', { z: 1.2, fx: 0.2, fy: 0.15 }],
        ['blitz', 14.5, 'Git log', { z: 1.25, fx: 0, fy: 0.1 }],
        ['blitz', 19.5, 'Keybinding editor', { z: 1.2, fx: 0.2, fy: 0.15 }],
        ['terminal', 16.8, 'Built-in terminal', { z: 1.35, fx: 1, fy: 0.05 }],
        ['terminal', 7.5, 'Split panes', { z: 1.0, fx: 0.5, fy: 0.3 }],
      ];
      if (beat < shots.length) {
        const [cn, rt, label, cam] = shots[beat];
        brandBug(1);
        drawWindow({ clip: clips[cn], r: rt + bl, title: 'fresh', cam, punch: 0.04 * Math.exp(-bl * 14), scale: clipScale(clips[cn]) });
        caption(label, bl, B + 0.2, { y: 360, size: 96, exit: false, stagger: 0.02 });
        if (bl < 0.07) { ctx.fillStyle = `rgba(255,255,255,${0.3 * (1 - bl / 0.07)})`; ctx.fillRect(0, 0, W, H); }
      } else {
        // rapid-fire 8th notes over black
        ctx.fillStyle = 'rgba(0,0,0,0.85)'; ctx.fillRect(0, 0, W, H);
        const words = [['LSP', C.green], ['SSH', C.blue], ['Vim mode', C.purple], ['Plugins', C.yellow]];
        const i = Math.min(3, Math.floor((lt - 6 * B) / (B / 2)));
        const wl = lt - 6 * B - i * B / 2;
        ctx.save(); shake(18 * Math.exp(-wl * 20), lt);
        slam(words[i][0], wl, W / 2, 960, i === 2 ? 190 : 260, words[i][1], (hash(i) - .5) * 0.12, { font: 'Inter', weight: 900, glitch: wl < 0.06 });
        ctx.restore();
      }
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
      const s = backOut(lt / 0.4);
      const size = 340 * s;
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
        const pulse = 0.5 + 0.5 * Math.sin(lt * 5);
        ctx.shadowColor = C.green; ctx.shadowBlur = 30 + 30 * pulse;
        rrect(-w / 2, -70, w, 140, 70); ctx.fillStyle = C.green; ctx.fill(); ctx.shadowBlur = 0;
        ctx.fillStyle = '#04120c'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
        ctx.fillText('getfresh.dev', 0, 4);
        ctx.restore();
      }
      if (lt > 4 * B) caption('Free & open source', lt - 4 * B, 99, { y: 1540, size: 46, color: C.text, exit: false, weight: 800 });
      if (lt > 5 * B) caption('Linux · macOS · Windows', lt - 5 * B, 99, { y: 1615, size: 40, color: C.muted, exit: false, weight: 800 });
    },
  },
];
