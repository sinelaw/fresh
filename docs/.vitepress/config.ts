import { defineConfig } from "vitepress";
import { genFeed } from "./genFeed";

export default defineConfig({
  title: "Fresh",
  description:
    "Fresh is a fast, modern terminal text editor with intuitive keybindings, syntax highlighting, and instant startup.",
  base: "/docs/",
  srcDir: ".",
  outDir: "../dist/docs",
  srcExclude: ["internal/**"],

  head: [
    ["link", { rel: "icon", href: "/docs/logo.svg" }],
    [
      "link",
      {
        rel: "alternate",
        type: "application/rss+xml",
        title: "Fresh Blog",
        href: "/docs/feed.rss",
      },
    ],
  ],

  cleanUrls: true,
  lastUpdated: true,
  appearance: "force-dark",

  buildEnd: genFeed,

  themeConfig: {
    logo: { light: "/logo.svg", dark: "/logo.svg" },

    nav: [
      { text: "Homepage", link: "https://getfresh.dev" },
      { text: "Getting Started", link: "/getting-started" },
      { text: "Blog", link: "/blog/" },
      { text: "Download", link: "https://github.com/sinelaw/fresh/releases/latest" },
    ],

    sidebar: [
      {
        text: "Blog",
        link: "/blog/",
        items: [
          { text: "The Architecture of Fresh", link: "/blog/fresh-pipeline/" },
          { text: "Fresh 0.3.0", link: "/blog/fresh-0.3.0/" },
          { text: "Fresh 0.2.18", link: "/blog/fresh-0.2.18/" },
          { text: "More…", link: "/blog/" },
        ],
      },
      {
        text: "Getting Started",
        link: "/getting-started/",
      },
      {
        text: "Features",
        items: [
          { text: "Overview", link: "/features/" },
          { text: "Editing", link: "/features/editing" },
          { text: "Command Palette", link: "/features/command-palette" },
          { text: "Navigation", link: "/features/navigation" },
          { text: "File Explorer", link: "/features/file-explorer" },
          { text: "Search and Replace", link: "/features/search-replace" },
          { text: "Integrated Terminal", link: "/features/terminal" },
          { text: "LSP Integration", link: "/features/lsp" },
          { text: "Git", link: "/features/git" },
          { text: "Themes", link: "/features/themes" },
          { text: "Encoding", link: "/features/encoding" },
          { text: "Remote Editing (SSH)", link: "/features/ssh" },
          { text: "Devcontainers", link: "/features/devcontainer" },
          { text: "Session Persistence", link: "/features/session-persistence" },
          { text: "Keybinding Editor", link: "/features/keybinding-editor" },
          { text: "Dashboard", link: "/features/dashboard" },
        ],
      },
      {
        text: "Guides",
        items: [
          { text: "Internationalization", link: "/i18n" },
          { text: "Privacy & Telemetry", link: "/privacy" },
          { text: "Troubleshooting", link: "/troubleshooting" },
        ],
      },
      {
        text: "Configuration",
        items: [
          { text: "Overview", link: "/configuration/" },
          { text: "Startup Script (init.ts)", link: "/configuration/init" },
          { text: "Keyboard", link: "/configuration/keyboard" },
        ],
      },
      {
        text: "Plugins",
        items: [
          { text: "Plugins", link: "/plugins/" },
          {
            text: "Development",
            collapsed: true,
            items: [
              { text: "Introduction", link: "/plugins/development/" },
              { text: "Language Packs", link: "/plugins/development/language-packs" },
              { text: "Common Patterns", link: "/plugins/development/patterns" },
              { text: "Utilities Library", link: "/plugins/development/utilities" },
              { text: "Internationalization", link: "/plugins/development/i18n" },
            ],
          },
          {
            text: "API Reference",
            collapsed: true,
            items: [
              { text: "Core Concepts & Types", link: "/plugins/api/" },
              { text: "Status & Logging", link: "/plugins/api/status-logging" },
              { text: "Buffer Operations", link: "/plugins/api/buffer" },
              { text: "Overlays & Virtual Text", link: "/plugins/api/overlays" },
              { text: "Filesystem & Paths", link: "/plugins/api/filesystem" },
              { text: "Events & Hooks", link: "/plugins/api/events" },
              { text: "Virtual Buffers", link: "/plugins/api/virtual-buffers" },
            ],
          },
        ],
      },
      {
        text: "Developer Docs",
        items: [
          { text: "Architecture", link: "/architecture" },
          { text: "WASM Compatibility", link: "/wasm" },
          { text: "QuickJS Migration", link: "/quickjs" },
          {
            text: "Visual Regression Tests",
            collapsed: true,
            items: [
              { text: "Comprehensive UI A", link: "/visual-regression/tests/Comprehensive_UI_A" },
              { text: "Comprehensive UI B", link: "/visual-regression/tests/Comprehensive_UI_B" },
            ],
          },
        ],
      },
    ],

    outline: { level: "deep" },

    socialLinks: [{ icon: "github", link: "https://github.com/sinelaw/fresh" }],

    search: { provider: "local" },

    editLink: {
      pattern: "https://github.com/sinelaw/fresh/edit/master/docs/:path",
    },

    footer: {
      message: "Released under the Apache 2.0 License",
    },
  },
});
