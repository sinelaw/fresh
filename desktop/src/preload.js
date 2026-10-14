'use strict';

// Runs before the web UI's own script on every page load. The web UI's default
// look ("Cosmos") draws a wallpaper and a hardware bezel around the editor —
// right for a browser tab, odd inside a window that already has a frame — so a
// desktop window starts on the chrome-light "Compact" theme. Ctrl+Alt+T and
// the theme menu still switch freely; the choice is the page's to remember.
try {
  if (location.protocol === 'http:' && !localStorage.getItem('fresh.webtheme')) {
    localStorage.setItem('fresh.webtheme', 'compact');
  }
} catch {
  // Storage unavailable: the page falls back to its own default.
}
