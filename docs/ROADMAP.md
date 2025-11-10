# Shortlist of Potential Issues & Improvement Areas

1. Incomplete Plugin System Integration: The foundation for a powerful Lua-based plugin system is impressively complete (PLUGIN_SYSTEM_IMPLEMENTATION.md). However, it is not yet integrated into the editor's main event loop. Critical APIs for buffer querying, async task management, and custom UI elements are designed but not yet exposed to plugins, preventing the creation of complex extensions like the envisioned Magit-style interface.
2. Unstable Overlays: The current overlay system, which uses absolute byte positions, is susceptible to "sliding" bugs when text is edited. This requires plugins to perform expensive manual re-scans. The marker_based_overlays.md design document correctly identifies this as a critical flaw that needs to be addressed to ensure plugin stability and performance.
3. Missing Core Editor Features: The editor lacks several features considered standard for a modern development environment. The TODO.md highlights the most significant gaps:
   - Search and Replace: While search is implemented, full replace functionality (including interactive replace, case/whole-word options, and history) is incomplete.
   - Smart Editing: Features like language-aware auto-indent, bracket matching/auto-pairing, and comment toggling are missing, impacting core editing flow and "feel."
4. Partial LSP Integration: The LSP architecture is robust, but support for many key features is not yet implemented. The absence of Hover Information, Code Actions, and Find References are major gaps for daily-driver usability.
5. High Barrier to Plugin Management: Drawing lessons from Neovim's "configuration tax," fresh currently has no mechanism for discovering, installing, or managing plugins beyond manually placing .lua files. This creates significant friction for users and hinders the growth of a plugin ecosystem.

______________________________________________________________________

# Plan to Stand Out

The competitive analysis reveals a clear opportunity: combine the performance of Rust-based editors with the extensibility of giants like VSCode and Neovim, while avoiding their primary pitfalls (Zed's lack of plugins, Helix's instability with large files, Neovim's configuration complexity).

fresh is uniquely positioned to achieve this. The following plan prioritizes work to leverage its architectural strengths and address its current gaps.

## Phase 1: Solidify the "Extensible Performance" Core (Immediate Priority)

Goal: Deliver on the promise of a fast, stable, and truly extensible editor.

1. Integrate the Plugin Engine: Connect the PluginManager to the editor's main loop. Implement the hook invocation points (before-save, after-insert, etc.) and process the plugin command queue on each frame. This single step makes the entire plugin infrastructure live.
2. Implement Marker-Based Overlays: Execute the plan in marker_based_overlays.md. This provides a stable foundation for all UI-related plugins (diagnostics, git blame, inline hints) and solves a difficult problem that many editors struggle with, making the fresh plugin API significantly more attractive and reliable for developers.
3. Expose Critical Plugin APIs: Implement the buffer query (get_buffer_content, get_cursor_position) and async task spawning APIs as designed in PLUGIN_SYSTEM_ANALYSIS.md. This unlocks the ability for plugins to read editor state and run external processes (e.g., git, linters) without blocking the UI.

## Phase 2: Achieve "Daily Driver" Feature Parity (Mid-Term Priority)

Goal: Close the core feature gaps that prevent developers from using `fresh` for day-to-day work.

1. Complete Search & Replace: Implement the remaining UI and logic for interactive search-and-replace (y/n/!/q), case/whole-word matching, and search history, as detailed in TODO.md.
2. Implement Smart Editing Essentials: Focus on the highest-impact "smart editing" features: language-aware auto-indent on newline and bracket auto-pairing. These have an outsized effect on the perceived quality of the editing experience.
3. Deliver High-Value LSP Features: Implement the "big three" of LSP functionality:
   - textDocument/hover (Hover Information)
   - textDocument/codeAction (Quick Fixes)
   - textDocument/references (Find References)

## Phase 3: Build a "Best-in-Class" User Experience (Long-Term Strategy)

Goal: Differentiate `fresh` by solving the usability and ecosystem problems that plague its competitors.

1. Build a Showcase Plugin: Develop a "killer app" plugin that is only possible due to fresh's unique architecture. The proposed Magit-style Git interface is the perfect candidate, as it requires custom UI, async processes, and tight editor integration. This will serve as both a powerful feature and the ultimate marketing for the plugin system.
2. Create a Seamless Plugin Manager: Design and implement a built-in UI for discovering, installing, updating, and configuring plugins. This directly attacks Neovim's primary weakness and mirrors a key strength of VSCode, making fresh far more approachable for the average user.
3. Cultivate an Ecosystem: With a stable, powerful, and easy-to-use plugin system, focus on community building. Create starter templates, write clear plugin development documentation (building on the excellent plugins/examples/README.md), and encourage the porting of popular plugins from other ecosystems.
