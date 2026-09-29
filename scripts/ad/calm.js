// The calm cut: 70 BPM, one long shot per bar (3.43 s), dips through the
// background between shots, serif captions, no flashes, shakes or key chips.

const K = {
  ink: '#0c0d0f', paper: '#ece9e2', mute: '#8a8d91', sage: '#a3c9ae',
};
const SERIF = (s) => `400 ${s}px Serif`;
const ITAL = '400 {size}px SerifI';

Object.assign(STYLE, {
  noFlash: true,
  windowGlow: 'rgba(0,0,0,0.65)', windowBlur: 80,
  dots: ['#3a3d42', '#3a3d42', '#3a3d42'],
  chrome: '#141518', border: 'rgba(255,255,255,0.07)',
  titleColor: '#6f7378', titleFont: '500 19px Inter',
  background(t) {
    ctx.fillStyle = K.ink; ctx.fillRect(0, 0, W, H);
    // one slow, soft light drifting across the top
    const x = W * (0.35 + 0.25 * Math.sin(t * 0.12)), y = H * (0.2 + 0.05 * Math.cos(t * 0.1));
    const g = ctx.createRadialGradient(x, y, 0, x, y, 1100);
    g.addColorStop(0, 'rgba(163,201,174,0.10)'); g.addColorStop(1, 'rgba(163,201,174,0)');
    ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
    const v = ctx.createRadialGradient(W / 2, H / 2, H * 0.3, W / 2, H / 2, H * 0.75);
    v.addColorStop(0, 'rgba(0,0,0,0)'); v.addColorStop(1, 'rgba(0,0,0,0.55)');
    ctx.fillStyle = v; ctx.fillRect(0, 0, W, H);
  },
});

// film grain, drawn last
function grain(t) {
  const f = Math.floor(t * 24);
  ctx.fillStyle = 'rgba(255,255,255,0.035)';
  for (let i = 0; i < 1400; i++) {
    const x = hash(f * 1.7 + i * 12.9898) * W, y = hash(f * 3.1 + i * 78.233) * H;
    ctx.fillRect(x, y, 1.6, 1.6);
  }
}

// fade in over the first 0.8 s of a shot, out over its last 0.6 s
function shotAlpha(lt, dur, inT = 0.8, outT = 0.6) {
  return easeInOut(lt / inT) * (1 - easeInOut((lt - (dur - outT)) / outT));
}

function softCaption(text, lt, dur, o = {}) {
  caption(text, lt, dur, Object.assign({
    y: 330, size: 84, font: 'Serif', weight: 400, color: K.paper, hi: K.sage, hiFont: ITAL,
    anim: 'soft', stagger: 0.09, lineGap: 1.12, delay: 0.25,
  }, o));
}

function label(text, lt, y, alpha = 1, size = 26, color = K.mute) {
  ctx.save();
  ctx.globalAlpha = alpha * easeInOut((lt - 0.1) / 0.9);
  ctx.font = `500 ${size}px Inter`; ctx.letterSpacing = size * 0.35 + 'px';
  ctx.fillStyle = color; ctx.textAlign = 'center'; ctx.textBaseline = 'middle';
  ctx.fillText(text, W / 2 + size * 0.175, y);
  ctx.restore();
}

// camera keyframes [[sceneT, {z, fx, fy}], ...], eased slowly between keys
function camPath(keys, lt, ease = 0.9) {
  let cur = keys[0][1];
  for (let i = 1; i < keys.length; i++) {
    const [kt, k] = keys[i];
    if (lt < kt) break;
    const f = easeInOut((lt - kt) / ease);
    cur = { z: lerp(cur.z, k.z, f), fx: lerp(cur.fx, k.fx, f), fy: lerp(cur.fy, k.fy, f) };
  }
  return cur;
}

// a terminal shot: slow push-in, gentle rise, dip to the background at both ends.
// All clips share one 140x75 screen; shots anchor left (fx 0) unless panning on purpose.
function calmShot(o) {
  return (lt, dur, t) => {
    const clip = clips[o.clip];
    const a = shotAlpha(lt, dur);
    const cam = o.cam ? camPath(o.cam, lt) : { z: lerp(o.z0, o.z1, easeInOut(lt / dur)), fx: 0, fy: 0 };
    drawWindow({
      clip, r: pmap(o.map, lt), title: o.title, alpha: a, dy: (1 - easeOut(lt / 1.2)) * 36,
      cam, scale: Math.min(TW / (clip.cols * CW), TH / (clip.rows * CHH)),
    });
    if (o.label) label(o.label, lt, 215, a);
    for (const [from, to, text] of o.captions) {
      if (lt >= from && lt < to) softCaption(text, lt - from, to - from, o.capOpts);
    }
    grain(t);
  };
}

// a slow ticker of every feature, in tracked caps between hairlines
const FEATURES = [
  'Command palette', 'Multi-cursor', 'Live grep', 'Themes', 'LSP', 'Go to definition',
  'Review diff', 'Git log', 'Git blame', 'Split panes', 'Integrated terminal', 'File explorer',
  'Keyboard macros', 'Vim mode', 'SSH remote editing', 'Multi-GB files', 'Hot exit',
  'Markdown compose', 'Code tours', 'TypeScript plugins', 'Settings UI', 'Keybinding editor',
  'Orchestrator', 'Git worktrees', 'Coding agents', 'Search & replace', 'Dev containers',
  'Mouse support', 'Autocomplete', 'Rename symbol', 'Diagnostics', 'Bookmarks', 'Rainbow brackets',
  'Emacs keys', 'Daemon mode', 'Sudo save', 'Flash jump', 'Package manager', 'Word wrap',
];
function ticker(lt, y, speed, offset, alpha) {
  ctx.save();
  ctx.globalAlpha = alpha;
  ctx.font = '500 24px Inter'; ctx.letterSpacing = '6px';
  ctx.fillStyle = '#9a9ca0'; ctx.textBaseline = 'middle';
  const text = FEATURES.slice(offset).concat(FEATURES.slice(0, offset)).map(f => f.toUpperCase()).join('   ·   ') + '   ·   ';
  const w = ctx.measureText(text).width;
  let x = -((lt * speed) % w);
  if (speed < 0) x = -w - ((lt * speed) % w);
  for (; x < W; x += w) ctx.fillText(text, x, y);
  ctx.restore();
  // soft edges
  for (const [x0, x1] of [[0, 160], [W, W - 160]]) {
    const g = ctx.createLinearGradient(x0, 0, x1, 0);
    g.addColorStop(0, K.ink); g.addColorStop(1, 'rgba(12,13,15,0)');
    ctx.fillStyle = g; ctx.fillRect(Math.min(x0, x1), y - 24, 160, 48);
  }
}

// Where the Orchestrator close-ups start in the agents recording (seconds):
// the dock switching workspaces, the split workspace, the main checkout's diff.
const ORCH = { dock: 173.4, split: 160.0, diff: 175.6 };

const SCENES = [
  // 0 — an opening line on black
  {
    start: 0, bars: 1,
    draw(lt, dur, t) {
      softCaption('Some tools simply\nget out of *your way.*', lt, dur, { y: 880, size: 92, delay: 0.5, stagger: 0.14 });
      grain(t);
    },
  },

  // 1 — the name
  {
    start: 1, bars: 1,
    draw(lt, dur, t) {
      const a = shotAlpha(lt, dur, 1.0, 0.7);
      ctx.save(); ctx.globalAlpha = a;
      const s = 230 * lerp(0.96, 1, easeOut(lt / 2));
      ctx.drawImage(imgs.logo, W / 2 - s / 2, 620 - s / 2, s, s);
      ctx.restore();
      softCaption('Fresh', lt, dur, { y: 960, size: 200, delay: 0.35, stagger: 0 });
      label('THE  TERMINAL  IDE', lt - 0.9, 1060, a);
      grain(t);
    },
  },

  // 2 — typing, at the speed you type
  {
    start: 2, bars: 1, clip: 'code',
    draw: calmShot({
      clip: 'code', title: 'main.rs', z0: 2.4, z1: 2.6,
      map: [[0, 8.55], [3.43, 12.0]],
      captions: [[0, 3.43, 'Familiar from the\n*first keystroke.*']],
    }),
  },

  // 3 — themes, crossfaded
  {
    start: 3, bars: 1, clip: 'themes',
    draw(lt, dur, t) {
      const clip = clips.themes;
      const a = shotAlpha(lt, dur);
      const z = lerp(2.0, 2.15, easeInOut(lt / dur));
      const looks = [11.6, 14.3, 15.2];          // nord, gruvbox, dracula (settled frames)
      const seg = dur / looks.length;
      const i = Math.min(looks.length - 1, Math.floor(lt / seg));
      const common = { title: 'server.rs', cam: { z, fx: 0, fy: 0 }, dy: (1 - easeOut(lt / 1.2)) * 36,
        scale: Math.min(TW / (clip.cols * CW), TH / (clip.rows * CHH)) };
      drawWindow(Object.assign({ clip, r: looks[i], alpha: a }, common));
      const x = (lt - (i + 1) * seg + 0.45) / 0.45;   // crossfade into the next look
      if (i + 1 < looks.length && x > 0) drawWindow(Object.assign({ clip, r: looks[i + 1], alpha: a * easeInOut(x) }, common));
      softCaption('Make it *yours.*', lt, dur);
      grain(t);
    },
  },

  // 4 — settings, without the config files
  {
    start: 4, bars: 1, clip: 'settings',
    draw: calmShot({
      clip: 'settings', title: 'settings', z0: 1.75, z1: 1.85,
      map: [[0, 11.0], [3.43, 16.3]],
      captions: [[0, 3.43, 'Settings, not\n*config files.*']],
    }),
  },

  // 5-6 — the Orchestrator, as three close-ups rather than one busy screen:
  // the dock's list of workspaces, an agent beside the code, the diff to review.
  {
    start: 5, bars: 2, clip: 'agents',
    draw(lt, dur, t) {
      const clip = clips.agents;
      const scale = Math.min(TW / (clip.cols * CW), TH / (clip.rows * CHH));
      const third = dur / 3;
      const subs = [
        { rec: ORCH.dock, cam: { z: 3.0, fx: 0, fy: 0 }, text: 'Every task gets its\nown *workspace.*' },
        { rec: ORCH.split, cam: { z: 1.2, fx: 1, fy: 0 }, text: 'An agent, right\nbeside *your code.*' },
        { rec: ORCH.diff, cam: { z: 2.0, fx: 0.336, fy: 0 }, text: 'Every change,\nready to *review.*' },
      ];
      const i = Math.min(2, Math.floor(lt / third));
      const sl = lt - i * third, sub = subs[i];
      const a = easeInOut(sl / 0.55) * (1 - easeInOut((sl - (third - 0.45)) / 0.45));
      const cam = { z: sub.cam.z * lerp(1, 1.04, sl / third), fx: sub.cam.fx, fy: sub.cam.fy };
      drawWindow({
        clip, r: sub.rec + sl, title: 'orchestrator', alpha: a, cam, scale,
        dy: (1 - easeOut(sl / 1.0)) * 24,
      });
      label('ORCHESTRATOR', lt, 215, shotAlpha(lt, dur, 0.8, 0.5));
      softCaption(sub.text, sl, third, { delay: 0.15, stagger: 0.06 });
      grain(t);
    },
  },

  // 7-8 — end card; six rows of features flow past underneath the whole time
  {
    start: 7, bars: 1.75,
    draw(lt, dur, t) {
      const out = 1 - easeInOut((lt - (dur - 0.9)) / 0.9);
      ctx.save(); ctx.globalAlpha = easeInOut(lt / 1.2) * out;
      const s = 150;
      ctx.drawImage(imgs.logo, W / 2 - s / 2, 470 - s / 2, s, s);
      ctx.restore();
      softCaption('Fresh', lt, 99, { y: 740, size: 160, delay: 0.3, stagger: 0, alpha: out });
      softCaption('The terminal IDE, *refined.*', lt, 99, { y: 845, size: 56, delay: 0.8, stagger: 0.07, color: '#b9b6b0', alpha: out });
      label('GETFRESH.DEV', lt - 1.5, 1010, out, 38, K.sage);
      ctx.save(); ctx.globalAlpha = easeInOut((lt - 1.6) / 0.9) * out;
      ctx.strokeStyle = 'rgba(236,233,226,0.25)'; ctx.lineWidth = 1.5;
      ctx.beginPath(); ctx.moveTo(W / 2 - 70, 1066); ctx.lineTo(W / 2 + 70, 1066); ctx.stroke();
      ctx.restore();
      softCaption('Free and open source.', lt, 99, { y: 1122, size: 40, delay: 2.0, stagger: 0.05, color: K.mute, alpha: out });
      const ta = easeInOut(lt / 0.8) * out;
      ctx.save(); ctx.globalAlpha = ta * 0.3; ctx.fillStyle = K.paper;
      ctx.fillRect(120, 1270, W - 240, 1); ctx.fillRect(120, 1790, W - 240, 1);
      ctx.restore();
      for (let row = 0; row < 6; row++) {
        const dir = row % 2 ? -1 : 1;
        ticker(lt + 2, 1340 + row * 80, dir * (38 + 7 * (row % 3)), (row * 9) % FEATURES.length, ta * (row % 2 ? 0.7 : 0.95));
      }
      grain(t);
    },
  },
];
