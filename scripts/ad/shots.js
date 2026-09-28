// Website stills: [clip, recording time, crop in cells [c0, r0, c1, r1]].
// All clips are 140x75; crops keep the interesting part legible on a page.
const SHOTS = {
  'orchestrator': { clip: 'agents', t: 160.0, crop: [0, 0, 140, 44] },
  'review-diff':  { clip: 'agents', t: 176.5, crop: [0, 0, 100, 31] },
  'settings':     { clip: 'settings', t: 16.6, crop: [0, 1, 132, 42] },
  'themes':       { clip: 'themes', t: 15.25, crop: [0, 0, 96, 30] },
  'multicursor':  { clip: 'multicursor', t: 17.2, crop: [0, 0, 84, 28] },
  'huge-file':    { clip: 'huge', t: 10.4, crop: [0, 0, 96, 30] },
  'palette':      { clip: 'palette', t: 8.9, crop: [0, 58, 100, 75] },
};
