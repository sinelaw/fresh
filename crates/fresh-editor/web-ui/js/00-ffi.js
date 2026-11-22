// Untyped escape hatches, declared for the inty checker in types/ffi.d.js.
// (web-ui/js — concatenated in filename order into the page's single
// <script> by crates/fresh-editor/build.rs; all files share one scope.)
// The only file web-ui/typecheck.sh does not check: everything here must be
// a line or two, with its type stated in types/ffi.d.js.
"use strict";
const listen = (target, type, fn, options) => target.addEventListener(type, fn, options);
const jsonAs = v => v;
const exposeTestHook = hook => { window.fresh = hook; };
const toFixed = (n, digits) => n.toFixed(digits);
// Web skins (js/15-theme.js): untrusted JSON read one checked step at a time,
// the editor theme's style rule, a file input's files, and HTML parsed inert.
const jsonParse = s => JSON.parse(s);
const jsonKind = v => v === null ? "null" : Array.isArray(v) ? "array" : typeof v;
const jsonGet = (v, k) => v !== null && typeof v === "object" && !Array.isArray(v) && Object.prototype.hasOwnProperty.call(v, k) ? v[k] : undefined;
const jsonItems = v => Array.isArray(v) ? v : [];
const jsonStr = v => typeof v === "string" ? v : null;
const editorThemeDecl = () => document.getElementById("editor-theme").sheet.cssRules[0].style;
const inputFiles = el => Array.from(el.files || []);
const fileDataUrl = f => new Promise((ok, bad) => { const r = new FileReader(); r.onload = () => ok(r.result); r.onerror = () => bad(r.error); r.readAsDataURL(f); });
const parseInertHtml = html => { const t = document.createElement("template"); t.innerHTML = html; return Array.from(t.content.childNodes); };
