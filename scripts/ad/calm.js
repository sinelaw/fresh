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

// a terminal shot: slow push-in, gentle rise, dip to the background at both ends
function calmShot(o) {
  return (lt, dur, t) => {
    const clip = clips[o.clip];
    const a = shotAlpha(lt, dur);
    const p = lt / dur;
    const z = lerp(o.z0 ?? 1.0, o.z1 ?? 1.1, easeInOut(p));
    drawWindow({
      clip, r: pmap(o.map, lt), title: o.title, alpha: a, dy: (1 - easeOut(lt / 1.2)) * 36,
      cam: { z, fx: o.fx ?? 0.5, fy: o.fy ?? 0.3 }, scale: Math.min(TW / (clip.cols * CW), TH / (clip.rows * CHH)),
    });
    if (o.label) label(o.label, lt, 215, a);
    softCaption(o.caption, lt, dur, o.capOpts);
    grain(t);
  };
}

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
      clip: 'code', title: 'main.rs', z0: 1.35, z1: 1.5, fx: 0.15, fy: 0.12,
      map: [[0, 8.55], [3.43, 12.0]],
      caption: 'Familiar from the\n*first keystroke.*',
    }),
  },

  // 3 — the palette
  {
    start: 3, bars: 1, clip: 'palette',
    draw: calmShot({
      clip: 'palette', title: 'server.rs', z0: 1.15, z1: 1.3, fx: 0.5, fy: 0.95,
      map: [[0, 4.1], [3.43, 7.6]],
      caption: 'Everything, one\nshortcut *away.*',
    }),
  },

  // 4 — themes, crossfaded
  {
    start: 4, bars: 1, clip: 'themes',
    draw(lt, dur, t) {
      const clip = clips.themes;
      const a = shotAlpha(lt, dur);
      const z = lerp(1.05, 1.15, easeInOut(lt / dur));
      const looks = [11.6, 14.3, 15.2];          // nord, gruvbox, dracula (settled frames)
      const seg = dur / looks.length;
      const i = Math.min(looks.length - 1, Math.floor(lt / seg));
      const common = { title: 'server.rs', cam: { z, fx: 0.3, fy: 0.25 }, dy: (1 - easeOut(lt / 1.2)) * 36 };
      drawWindow(Object.assign({ clip, r: looks[i], alpha: a }, common));
      const x = (lt - (i + 1) * seg + 0.45) / 0.45;   // crossfade into the next look
      if (i + 1 < looks.length && x > 0) drawWindow(Object.assign({ clip, r: looks[i + 1], alpha: a * easeInOut(x) }, common));
      softCaption('Make it *yours.*', lt, dur);
      grain(t);
    },
  },

  // 5 — the Orchestrator
  {
    start: 5, bars: 1, clip: 'agents',
    draw: calmShot({
      clip: 'agents', title: 'orchestrator', z0: 1.12, z1: 1.2, fx: 0, fy: 0,
      map: [[0, 96.7], [3.43, 100.2]],
      label: 'ORCHESTRATOR',
      caption: 'Your agents,\nside by *side.*',
    }),
  },

  // 6 — the two-gigabyte log
  {
    start: 6, bars: 1, clip: 'huge',
    draw: calmShot({
      clip: 'huge', title: 'huge.log', z0: 1.45, z1: 1.55, fx: 0, fy: 0,
      map: [[0, 3.85], [0.95, 4.9], [1.6, 5.65], [2.15, 6.2], [2.2, 9.6], [3.43, 10.9]],
      caption: 'Two gigabytes.\n*Unbothered.*',
    }),
  },

  // 7-8 — end card
  {
    start: 7, bars: 1.75,
    draw(lt, dur, t) {
      const out = 1 - easeInOut((lt - (dur - 1.0)) / 1.0);
      ctx.save(); ctx.globalAlpha = easeInOut(lt / 1.2) * out;
      const s = 170;
      ctx.drawImage(imgs.logo, W / 2 - s / 2, 660 - s / 2, s, s);
      ctx.restore();
      softCaption('Fresh', lt, 99, { y: 960, size: 170, delay: 0.3, stagger: 0, alpha: out });
      softCaption('The terminal IDE, *refined.*', lt, 99, { y: 1070, size: 58, delay: 0.9, stagger: 0.07, color: '#b9b6b0', alpha: out });
      label('GETFRESH.DEV', lt - 1.8, 1330, out, 38, K.sage);
      ctx.save(); ctx.globalAlpha = easeInOut((lt - 1.9) / 0.9) * out;
      ctx.strokeStyle = 'rgba(236,233,226,0.25)'; ctx.lineWidth = 1.5;
      ctx.beginPath(); ctx.moveTo(W / 2 - 70, 1392); ctx.lineTo(W / 2 + 70, 1392); ctx.stroke();
      ctx.restore();
      softCaption('Free and open source.', lt, 99, { y: 1450, size: 40, delay: 2.4, stagger: 0.05, color: K.mute, alpha: out });
      grain(t);
    },
  },
];
