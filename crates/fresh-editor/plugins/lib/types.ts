/// <reference path="./fresh.d.ts" />

/**
 * Shared Types for Fresh Editor Plugin Library
 *
 * Common interfaces and types used across LSP-related plugins.
 */

/**
 * RGB color tuple for overlays and highlighting
 */
export type RGB = [number, number, number];

/**
 * File explorer decoration metadata provided by plugins
 */
export interface FileExplorerDecoration {
  /** Absolute or workspace-relative path to decorate */
  path: string;
  /** Symbol to display (single character recommended) */
  symbol?: string;
  /** RGB color for the symbol */
  color?: RGB;
  /** Priority for resolving conflicts (higher wins) */
  priority?: number;
}

/**
 * File location with line and column
 */
export interface Location {
  file: string;
  line: number;
  column: number;
}

/**
 * Options for opening a panel
 */
export interface PanelOptions {
  /** Text property entries to display */
  entries: TextPropertyEntry[];
  /** Split ratio (0.0 to 1.0), default 0.3 */
  ratio?: number;
  /** Whether to show line numbers, default false for panels */
  showLineNumbers?: boolean;
  /** Whether editing is disabled, default true for panels */
  editingDisabled?: boolean;
}

/**
 * State of a managed panel
 */
export interface PanelState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceSplitId: number | null;
  sourceBufferId: number | null;
}

/**
 * Options for NavigationController
 */
export interface NavigationOptions<T> {
  /** Function to call when selection changes */
  onSelectionChange?: (item: T, index: number) => void;
  /** Label for status messages (e.g., "Diagnostic", "Reference") */
  itemLabel?: string;
  /** Whether to wrap around at boundaries */
  wrap?: boolean;
}

/**
 * Highlight pattern for syntax highlighting
 */
export interface HighlightPattern {
  /** Function to test if line matches */
  match: (line: string) => boolean;
  /** Color to apply */
  rgb: RGB;
  /** Whether to underline */
  underline?: boolean;
  /** Prefix for overlay IDs */
  overlayIdPrefix?: string;
}
