// Render ad.html frame by frame with headless Chromium.
// Run from the work dir while it is served on $AD_PORT (default 8765); build.sh does both.
// usage: node render.mjs stills t1 t2 ...   |  FFMPEG=/path/ffmpeg node render.mjs video out.mp4
import { spawn, execSync } from 'child_process';
let chromium;
try { ({ chromium } = await import('playwright')); }
catch { ({ chromium } = await import(execSync('npm root -g').toString().trim() + '/playwright/index.mjs')); }
import fs from 'fs';
const [mode, ...rest] = process.argv.slice(2);
const FF = process.env.FFMPEG;
const browser = await chromium.launch({ args: ['--disable-web-security'] });
const page = await browser.newPage({ viewport: { width: 1080, height: 1920 } });
page.on('console', m => console.log('page:', m.text()));
page.on('pageerror', e => console.log('pageerror:', e.message));
await page.goto(`http://127.0.0.1:${process.env.AD_PORT || 8765}/ad.html`);
await page.evaluate(() => window.ready);
const grab = async t => {
  const b64 = await page.evaluate(t => { window.renderAt(t); return document.getElementById('c').toDataURL('image/png').split(',')[1]; }, t);
  return Buffer.from(b64, 'base64');
};
if (mode === 'stills') {
  fs.mkdirSync('stills', { recursive: true });
  for (const t of rest) fs.writeFileSync(`stills/${t}.png`, await grab(parseFloat(t)));
} else {
  const out = rest[0], fps = 30, n = Math.round(30 * fps);
  const ff = spawn(FF, ['-y', '-loglevel', 'error', '-f', 'image2pipe', '-framerate', String(fps), '-i', '-',
    '-i', 'music.wav', '-map', '0:v', '-map', '1:a', '-c:v', 'libx264', '-preset', 'slow', '-crf', '17',
    '-pix_fmt', 'yuv420p', '-profile:v', 'high', '-c:a', 'aac', '-b:a', '192k', '-shortest', '-movflags', '+faststart', out],
    { stdio: ['pipe', 'inherit', 'inherit'] });
  for (let i = 0; i < n; i++) {
    const buf = await grab(i / fps);
    if (!ff.stdin.write(buf)) await new Promise(r => ff.stdin.once('drain', r));
    if (i % 60 === 0) console.log('frame', i);
  }
  ff.stdin.end();
  await new Promise(r => ff.on('close', r));
}
await browser.close();
