/// <reference path="./lib/fresh.d.ts" />

/**
 * Nerd Font file-type icons for the File Explorer.
 *
 * Core owns only the path-independent rule evaluator. This plugin owns the
 * visual policy and registers it when `editor.nerd_font_icons` is enabled.
 */

const editor = getEditor();
const NAMESPACE = "file-type-icons";
const filenameColor = { source: "filename" as const };
const icon = (text: string): FileExplorerLeadingRuleSlot => ({
  text,
  color: filenameColor,
  minWidth: 1,
});

const rules: FileExplorerLeadingSlotRules = {
  priority: 0,
  caseSensitive: false,
  exactFiles: {
    ".gitignore": icon("\ue702"),
    ".gitattributes": icon("\ue702"),
    ".gitmodules": icon("\ue702"),
    "Dockerfile": icon("\ue7b0"),
    "Containerfile": icon("\ue7b0"),
    "docker-compose.yml": icon("\ue7b0"),
    "docker-compose.yaml": icon("\ue7b0"),
    "Cargo.toml": icon("\ue7a8"),
    "Cargo.lock": icon("\ue7a8"),
    "rust-toolchain": icon("\ue7a8"),
    "rust-toolchain.toml": icon("\ue7a8"),
    "go.mod": icon("\ue627"),
    "go.sum": icon("\ue627"),
    "go.work": icon("\ue627"),
    "package.json": icon("\ue718"),
    "package-lock.json": icon("\ue718"),
    "yarn.lock": icon("\ue718"),
    "pnpm-lock.yaml": icon("\ue718"),
    "bun.lock": icon("\ue718"),
    "Makefile": icon("\ue615"),
    "GNUmakefile": icon("\ue615"),
    "CMakeLists.txt": icon("\ue615"),
    "Justfile": icon("\ue615"),
    "Taskfile.yml": icon("\ue615"),
    "Taskfile.yaml": icon("\ue615"),
    "README": icon("\ue609"),
    "README.md": icon("\ue609"),
    "README.mdx": icon("\ue609"),
    "README.markdown": icon("\ue609"),
    "LICENSE": icon("\uf24e"),
    "LICENSE.md": icon("\uf24e"),
    "LICENSE.txt": icon("\uf24e"),
    "COPYING": icon("\uf24e"),
  },
  directoryNames: {},
  extensions: {
    rs: icon("\ue7a8"),
    js: icon("\ue74e"), mjs: icon("\ue74e"), cjs: icon("\ue74e"),
    ts: icon("\ue628"), mts: icon("\ue628"), cts: icon("\ue628"),
    jsx: icon("\ue7ba"), tsx: icon("\ue7ba"),
    vue: icon("\ue6a0"),
    py: icon("\ue73c"), pyi: icon("\ue73c"), pyw: icon("\ue73c"),
    go: icon("\ue627"),
    lua: icon("\ue620"),
    rb: icon("\ue791"), gemspec: icon("\ue791"),
    php: icon("\ue73d"),
    java: icon("\ue738"), class: icon("\ue738"), jar: icon("\ue738"),
    kt: icon("\ue634"), kts: icon("\ue634"),
    swift: icon("\ue755"),
    cs: icon("\udb80\udf1b"),
    c: icon("\ue61e"), h: icon("\ue61e"),
    cc: icon("\ue61d"), cpp: icon("\ue61d"), cxx: icon("\ue61d"),
    hh: icon("\ue61d"), hpp: icon("\ue61d"), hxx: icon("\ue61d"),
    ex: icon("\ue62d"), exs: icon("\ue62d"),
    erl: icon("\ue7b1"), hrl: icon("\ue7b1"),
    hs: icon("\ue777"), lhs: icon("\ue777"),
    scala: icon("\ue737"),
    clj: icon("\ue768"), cljs: icon("\ue768"), cljc: icon("\ue768"), edn: icon("\ue768"),
    sh: icon("\ue795"), bash: icon("\ue795"), zsh: icon("\ue795"),
    fish: icon("\ue795"), nu: icon("\ue795"),
    html: icon("\ue736"), htm: icon("\ue736"),
    css: icon("\ue749"),
    scss: icon("\ue74b"), sass: icon("\ue74b"),
    less: icon("\ue758"),
    md: icon("\ue609"), mdx: icon("\ue609"), markdown: icon("\ue609"),
    json: icon("\ue60b"), jsonc: icon("\ue60b"), json5: icon("\ue60b"),
    yaml: icon("\ue6a8"), yml: icon("\ue6a8"),
    toml: icon("\ue615"), ini: icon("\ue615"), cfg: icon("\ue615"),
    conf: icon("\ue615"), config: icon("\ue615"),
    xml: icon("\uf72d"), xsl: icon("\uf72d"), xslt: icon("\uf72d"), svg: icon("\uf72d"),
    sql: icon("\uf1c0"), sqlite: icon("\uf1c0"), db: icon("\uf1c0"),
    graphql: icon("\ue662"), gql: icon("\ue662"),
    proto: icon("\ue6a1"),
    lock: icon("\uf023"),
    png: icon("\uf1c5"), jpg: icon("\uf1c5"), jpeg: icon("\uf1c5"),
    gif: icon("\uf1c5"), webp: icon("\uf1c5"), ico: icon("\uf1c5"),
    bmp: icon("\uf1c5"), tif: icon("\uf1c5"), tiff: icon("\uf1c5"),
    mp3: icon("\uf1c7"), wav: icon("\uf1c7"), flac: icon("\uf1c7"),
    ogg: icon("\uf1c7"), m4a: icon("\uf1c7"), aac: icon("\uf1c7"),
    mp4: icon("\uf1c8"), mkv: icon("\uf1c8"), mov: icon("\uf1c8"),
    webm: icon("\uf1c8"), avi: icon("\uf1c8"),
    zip: icon("\uf1c6"), tar: icon("\uf1c6"), gz: icon("\uf1c6"),
    bz2: icon("\uf1c6"), xz: icon("\uf1c6"), "7z": icon("\uf1c6"),
    rar: icon("\uf1c6"), tgz: icon("\uf1c6"),
    pdf: icon("\uf1c1"),
    txt: icon("\uf15c"), log: icon("\uf15c"),
  },
  fallbackFile: icon("\uf15b"),
  fallbackDirectory: icon("\uf07b"),
};

function enabled(): boolean {
  const config = editor.getConfig() as { editor?: { nerd_font_icons?: boolean } };
  return config.editor?.nerd_font_icons === true;
}

function refresh(): void {
  if (enabled()) {
    editor.setFileExplorerLeadingSlotRules(NAMESPACE, rules);
  } else {
    editor.clearFileExplorerLeadingSlotRules(NAMESPACE);
  }
}

editor.on("editor_initialized", refresh);
editor.on("config_changed", refresh);
