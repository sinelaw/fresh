// What this browser remembers of a workspace between visits: the chrome's
// sizes, which parts were open, the open files and how the splits held
// them. Kept in localStorage under the workspace's host and root, so two
// workspaces (or one served from two hosts) keep their own.

// The shape it is kept in names the key: data kept in another shape is
// left alone (not read).
const PREFIX = "fresh-poc:session2:";

/** function emptySession() => SessionData */
function emptySession() { return { layout: null, recent: [], sizes: {} }; }
// What was kept under `key`, or nothing kept.
/** function readSession(key: String) => SessionData */
function readSession(key) {
  try {
    const text = localStorage.getItem(key);
    if (text === null) return emptySession();
    /** const kept: SessionData */
    const kept = JSON.parse(text);
    return kept;
  } catch { return emptySession(); }
}

export class Session {
  constructor() {
    this.keys = []; // the workspace's key, once opened: a list of at most one
    this.data = emptySession();
    this.timer = 0;
  }

  // Start remembering for the workspace `root` on `host`; reads what was
  // remembered last time.
  open(host, root) {
    const key = `${PREFIX}${host}:${root}`;
    this.keys = [key];
    this.data = readSession(key);
  }

  get layout() { return this.data.layout; }
  get recent() { return this.data.recent; }
  get sizes() { return this.data.sizes; }
  // Remember the layout, the recent files or the parts' sizes; written
  // shortly after (a drag or a burst of tab changes writes once).
  setLayout(layout) { this.data = { layout, recent: this.data.recent, sizes: this.data.sizes }; this.later(); }
  setRecent(recent) { this.data = { layout: this.data.layout, recent, sizes: this.data.sizes }; this.later(); }
  setSizes(sizes) { this.data = { layout: this.data.layout, recent: this.data.recent, sizes }; this.later(); }

  later() {
    clearTimeout(this.timer);
    this.timer = setTimeout(() => { this.flush(); }, 300);
  }

  flush() {
    clearTimeout(this.timer);
    for (const key of this.keys) {
      try { localStorage.setItem(key, JSON.stringify(this.data)); } catch {}
    }
  }
}
