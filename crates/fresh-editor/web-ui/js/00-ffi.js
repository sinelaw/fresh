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
