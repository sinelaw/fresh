// What this browser remembers of a workspace between visits: the chrome's
// sizes, which parts were open, the open files and how the splits held
// them. Kept in localStorage under the workspace's host and root, so two
// workspaces (or one served from two hosts) keep their own.

const PREFIX = "fresh-poc:workspace:";

export class Session {
  constructor() {
    this.key = "";
    this.data = {};
    this.timer = 0;
  }

  // Start remembering for the workspace `root` on `host`; reads what was
  // remembered last time.
  open(host, root) {
    this.key = `${PREFIX}${host}:${root}`;
    try { this.data = JSON.parse(localStorage.getItem(this.key) || "{}") || {}; } catch { this.data = {}; }
  }

  get(k) { return this.data[k]; }

  // Remember `v` under `k`; written shortly after (a drag or a burst of tab
  // changes writes once).
  set(k, v) {
    this.data[k] = v;
    clearTimeout(this.timer);
    this.timer = setTimeout(() => this.flush(), 300);
  }

  flush() {
    clearTimeout(this.timer);
    if (!this.key) return;
    try { localStorage.setItem(this.key, JSON.stringify(this.data)); } catch {}
  }
}
