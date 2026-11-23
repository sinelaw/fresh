'use strict';

// Runs before the web client's script. A browser hands a dropped file over as
// contents only; inside the desktop app the file has a path on this machine,
// and opening it by that path edits the file itself rather than a copy.
const { contextBridge, webUtils } = require('electron');

contextBridge.exposeInMainWorld('freshDesktop', {
  pathForFile: (file) => {
    try {
      return webUtils.getPathForFile(file) || null;
    } catch {
      return null;
    }
  },
});

// The web client's default skin ("Cosmos") draws a wallpaper and a hardware
// bezel around the editor — right for a browser tab, odd inside a window that
// already has a frame — so a desktop window starts on the quiet "Compact"
// skin. Ctrl+Alt+T and the skin picker still switch freely; the choice is the
// page's to remember.
try {
  if (location.protocol === 'http:' && !localStorage.getItem('fresh.webtheme')) {
    localStorage.setItem('fresh.webtheme', 'compact');
  }
} catch {
  // Storage unavailable: the page falls back to its own default.
}
