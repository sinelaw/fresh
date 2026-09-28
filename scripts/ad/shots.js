// Website stills: [clip, recording time, crop in cells [c0, r0, c1, r1]].
// All clips are 140x75; crops keep the interesting part legible on a page.
const SHOTS = {
  // also the poster of the looping dock walk (site.sh renders the clip)
  'orchestrator': { clip: 'switch', t: 3.0, crop: [0, 0, 140, 44] },
  'review-diff':  { clip: 'agents', t: 176.5, crop: [0, 0, 100, 31] },
  'settings':     { clip: 'settings', t: 16.6, crop: [0, 1, 132, 42] },
  // one still per theme for the landing page's crossfade (settled after Fresh's own fade)
  'theme-tokyo-night': { clip: 'themes', t: 8.3, crop: [0, 0, 96, 30] },
  'theme-gruvbox': { clip: 'themes', t: 14.7, crop: [0, 0, 96, 30] },
  'theme-nord': { clip: 'themes', t: 12.0, crop: [0, 0, 96, 30] },
  'theme-light': { clip: 'themes', t: 12.9, crop: [0, 0, 96, 30] },
  'theme-dracula': { clip: 'themes', t: 15.6, crop: [0, 0, 96, 30] },
  'theme-solarized-dark': { clip: 'themes', t: 10.2, crop: [0, 0, 96, 30] },
  'multicursor':  { clip: 'multicursor', t: 17.2, crop: [0, 0, 84, 28] },
  'huge-file':    { clip: 'huge', t: 10.4, crop: [0, 0, 96, 30] },
  'palette':      { clip: 'palette', t: 8.9, crop: [0, 58, 100, 75] },
};
