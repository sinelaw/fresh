// Fresh Editor TypeScript Plugin Bootstrap
// This file sets up the global 'editor' API object that plugins can use

const core = Deno.core;

// Create the editor API object
const editor = {
  // Status bar
  setStatus(message) {
    core.ops.op_fresh_set_status(message);
  },

  getCurrentLocale() {
    return core.ops.op_fresh_get_current_locale();
  },

  t(key, args = {}, pluginName = null) {
    const source = pluginName || globalThis.__PLUGIN_SOURCE__;
    if (!source) {
      editor.debug(`editor.t("${key}") called without plugin context. Pass plugin name or use getL10n().`);
      return key;
    }
    return core.ops.op_fresh_plugin_translate(source, key, args);
  },

  getL10n() {
    const source = globalThis.__PLUGIN_SOURCE__;
    return {
      t: (key, args = {}) => this.t(key, args, source)
    };
  },

  // Logging - all methods include plugin source context
  error(message) {
    const source = globalThis.__PLUGIN_SOURCE__ || 'unknown';
    core.ops.op_fresh_error(`[${source}] ${message}`);
  },
  warn(message) {
    const source = globalThis.__PLUGIN_SOURCE__ || 'unknown';
    core.ops.op_fresh_warn(`[${source}] ${message}`);
  },
  info(message) {
    const source = globalThis.__PLUGIN_SOURCE__ || 'unknown';
    core.ops.op_fresh_info(`[${source}] ${message}`);
  },
  debug(message) {
    const source = globalThis.__PLUGIN_SOURCE__ || 'unknown';
    core.ops.op_fresh_debug(`[${source}] ${message}`);
  },

  // Buffer operations (placeholders for now)
  getActiveBufferId() {
    return core.ops.op_fresh_get_active_buffer_id();
  },

  // TODO: Add more ops as they are implemented in Rust
  // - getBufferInfo
  // - insertText
  // - deleteRange
  // - addOverlay
  // - removeOverlay
  // - registerCommand
  // - defineMode
  // - createVirtualBufferInSplit (async)
  // - spawn (async)
  // - openFile (async)
};

// Make editor globally available
globalThis.editor = editor;

// Override console to route through editor logging instead of stderr
globalThis.console = {
  log: (...args) => editor.info(args.map(a => String(a)).join(' ')),
  warn: (...args) => editor.warn(args.map(a => String(a)).join(' ')),
  error: (...args) => editor.error(args.map(a => String(a)).join(' ')),
  info: (...args) => editor.info(args.map(a => String(a)).join(' ')),
  debug: (...args) => editor.debug(args.map(a => String(a)).join(' ')),
};

// Log that the runtime is ready
editor.debug("Fresh TypeScript plugin runtime initialized");
