import { defineConfig } from "vitepress";

export default defineConfig({
  title: "Fresh",
  description:
    "Fresh is a fast, modern terminal text editor with intuitive keybindings, syntax highlighting, and instant startup.",
  base: "/docs/",
  srcDir: ".",
  outDir: "../dist/docs",

  head: [["link", { rel: "icon", href: "/docs/logo.svg" }]],

  cleanUrls: true,
  lastUpdated: true,
  appearance: "force-dark",
  themeConfig: {
    logo: { light: "/logo.svg", dark: "/logo.svg" },

    nav: [
      { text: "Homepage", link: "https://getfresh.dev" },
      { text: "Getting Started", link: "/getting-started" },
      { text: "Download", link: "https://github.com/sinelaw/fresh/releases/latest" },
    ],

    sidebar: [
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
          { text: "Themes", link: "/features/themes" },
          { text: "Encoding", link: "/features/encoding" },
          { text: "Remote Editing (SSH)", link: "/features/ssh" },
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
            text: "Design Docs",
            collapsed: true,
            items: [
              { text: "Finder Abstraction", link: "/design/finder-abstraction" },
              { text: "Search Next Occurrence", link: "/design/search-next-occurrence" },
            ],
          },
          {
            text: "Internal",
            collapsed: true,
            items: [
              {
                text: "Configuration",
                collapsed: true,
                items: [
                  { text: "Config Design", link: "/internal/config-design" },
                  { text: "Config Editor Design", link: "/internal/config-editor-design" },
                  { text: "Config Implementation", link: "/internal/config-implementation-plan" },
                  { text: "Settings Modified Indicator", link: "/internal/settings-modified-indicator-design" },
                ],
              },
              {
                text: "Plugins",
                collapsed: true,
                items: [
                  { text: "Plugin Architecture", link: "/internal/plugin-architecture-plan" },
                  { text: "Plugin Marketplace", link: "/internal/plugin-marketplace-design" },
                  { text: "Plugin Usability", link: "/internal/plugin-usability-review" },
                  { text: "i18n Design", link: "/internal/i18n-design" },
                ],
              },
              {
                text: "Themes",
                collapsed: true,
                items: [
                  { text: "Theme Consolidation", link: "/internal/theme-consolidation-plan" },
                  { text: "Theme Usability", link: "/internal/theme-usability-improvements" },
                  { text: "Theme User Flows", link: "/internal/theme-user-flows" },
                ],
              },
              {
                text: "UI/Input",
                collapsed: true,
                items: [
                  { text: "Input Calibration", link: "/internal/input-calibration-wizard" },
                  { text: "Event Dispatch", link: "/internal/event-dispatch-architecture" },
                  { text: "Paste Handling", link: "/internal/paste-handling" },
                  { text: "Vi Mode Design", link: "/internal/vi-mode-design" },
                  { text: "Warning UX", link: "/internal/warning-notification-ux" },
                ],
              },
              {
                text: "View/Rendering",
                collapsed: true,
                items: [
                  { text: "Diff View", link: "/internal/diff-view" },
                  { text: "Markdown", link: "/internal/markdown" },
                  { text: "Scroll Sync", link: "/internal/scroll-sync-design" },
                  { text: "Visual Layout", link: "/internal/visual-layout-unification" },
                ],
              },
              {
                text: "Terminal",
                collapsed: true,
                items: [
                  { text: "Terminal Design", link: "/internal/terminal" },
                  { text: "I/O Separation", link: "/internal/io-separation-plan" },
                ],
              },
              {
                text: "Remote",
                collapsed: true,
                items: [
                  { text: "SSH Remote Editing Design", link: "/internal/ssh-remote-editing-design" },
                ],
              },
              {
                text: "Core/System",
                collapsed: true,
                items: [
                  { text: "Testing Guide", link: "/internal/testing" },
                  { text: "Bulk Edit Optimization", link: "/internal/bulk-edit-optimization" },
                  { text: "Code Review", link: "/internal/code-review" },
                  { text: "TimeSource Design", link: "/internal/timesource-design" },
                  { text: "Unicode Width", link: "/internal/unicode-width" },
                  { text: "Readme", link: "/internal/README" },

                ],
              },
            ],
          },
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
