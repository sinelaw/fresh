/**
 * Fresh Editor Plugin Library
 *
 * Shared utilities for building LSP-related plugins with common patterns.
 *
 * @example
 * ```typescript
 * import { PanelManager, NavigationController, VirtualBufferFactory } from "./lib/index.ts";
 * import type { Location, RGB, PanelOptions } from "./lib/index.ts";
 * ```
 */

// Types
export type {
  RGB,
  Location,
  PanelOptions,
  PanelState,
  NavigationOptions,
  HighlightPattern,
  FileExplorerDecoration,
} from "./types.ts";

// Panel Management
export { PanelManager } from "./panel-manager.ts";

// Navigation
export { NavigationController } from "./navigation-controller.ts";

// Buffer Creation
export { createVirtualBufferFactory } from "./virtual-buffer-factory.ts";
export type { VirtualBufferOptions, SplitBufferOptions } from "./virtual-buffer-factory.ts";

// Finder Abstraction
export { Finder, defaultFuzzyFilter, parseGrepLine, parseGrepOutput, getRelativePath, createLiveProvider } from "./finder.ts";
export type {
  DisplayEntry,
  SearchSource,
  FilterSource,
  PreviewConfig,
  FinderConfig,
  PromptOptions,
  PanelOptions as FinderPanelOptions,
  FinderProvider,
  LivePanelOptions,
} from "./finder.ts";
