/// <reference path="./fresh.d.ts" />

import type { NavigationOptions } from "./types.ts";

/**
 * NavigationController - Generic list navigation for panel plugins
 *
 * Handles the common pattern of:
 * - Maintaining selected index
 * - Boundary checking
 * - Status message updates
 * - Callback on selection change
 *
 * @example
 * ```typescript
 * const nav = new NavigationController<DiagnosticItem>({
 *   itemLabel: "Diagnostic",
 *   onSelectionChange: (item, index) => {
 *     updateHighlight(index);
 *   }
 * });
 *
 * // Set items when panel opens
 * nav.setItems(diagnostics);
 *
 * // Navigation commands
 * function next() { nav.next(); }
 * function prev() { nav.prev(); }
 * ```
 */
export class NavigationController<T> {
  private items: T[] = [];
  private currentIndex: number = 0;
  private options: NavigationOptions<T>;

  constructor(private readonly editor: EditorAPI, options: NavigationOptions<T> = {}) {
    this.options = {
      itemLabel: "Item",
      wrap: false,
      ...options,
    };
  }

  /**
   * Set the items to navigate through
   *
   * @param items - Array of items
   * @param resetIndex - Whether to reset index to 0 (default true)
   */
  setItems(items: T[], resetIndex: boolean = true): void {
    this.items = items;
    if (resetIndex) {
      this.currentIndex = 0;
    } else {
      // Clamp to valid range
      this.currentIndex = Math.min(this.currentIndex, Math.max(0, items.length - 1));
    }
  }

  /**
   * Get all items
   */
  getItems(): T[] {
    return this.items;
  }

  /**
   * Get the current selected index
   */
  get selectedIndex(): number {
    return this.currentIndex;
  }

  /**
   * Set the selected index directly
   */
  set selectedIndex(index: number) {
    if (this.items.length === 0) return;
    this.currentIndex = Math.max(0, Math.min(index, this.items.length - 1));
    this.notifyChange();
  }

  /**
   * Get the currently selected item
   */
  get selected(): T | null {
    if (this.items.length === 0 || this.currentIndex >= this.items.length) {
      return null;
    }
    return this.items[this.currentIndex];
  }

  /**
   * Get the total number of items
   */
  get count(): number {
    return this.items.length;
  }

  /**
   * Check if there are any items
   */
  get isEmpty(): boolean {
    return this.items.length === 0;
  }

  /**
   * Move to the next item
   */
  next(): void {
    if (this.items.length === 0) return;

    if (this.options.wrap) {
      this.currentIndex = (this.currentIndex + 1) % this.items.length;
    } else {
      this.currentIndex = Math.min(this.currentIndex + 1, this.items.length - 1);
    }
    this.notifyChange();
  }

  /**
   * Move to the previous item
   */
  prev(): void {
    if (this.items.length === 0) return;

    if (this.options.wrap) {
      this.currentIndex = (this.currentIndex - 1 + this.items.length) % this.items.length;
    } else {
      this.currentIndex = Math.max(this.currentIndex - 1, 0);
    }
    this.notifyChange();
  }

  /**
   * Move to the first item
   */
  first(): void {
    if (this.items.length === 0) return;
    this.currentIndex = 0;
    this.notifyChange();
  }

  /**
   * Move to the last item
   */
  last(): void {
    if (this.items.length === 0) return;
    this.currentIndex = this.items.length - 1;
    this.notifyChange();
  }

  /**
   * Jump to a specific index
   *
   * @param index - Target index
   */
  jumpTo(index: number): void {
    if (this.items.length === 0) return;
    this.currentIndex = Math.max(0, Math.min(index, this.items.length - 1));
    this.notifyChange();
  }

  /**
   * Update the status message with current position
   *
   * @param customMessage - Optional custom message (overrides default)
   */
  showStatus(customMessage?: string): void {
    if (this.items.length === 0) {
      this.editor.setStatus(`No ${this.options.itemLabel}s`);
      return;
    }

    const message = customMessage ||
      `${this.options.itemLabel} ${this.currentIndex + 1}/${this.items.length}`;
    this.editor.setStatus(message);
  }

  /**
   * Reset the controller state
   */
  reset(): void {
    this.items = [];
    this.currentIndex = 0;
  }

  /**
   * Find and select an item matching a predicate
   *
   * @param predicate - Function to test items
   * @returns true if found and selected, false otherwise
   */
  findAndSelect(predicate: (item: T) => boolean): boolean {
    const index = this.items.findIndex(predicate);
    if (index !== -1) {
      this.currentIndex = index;
      this.notifyChange();
      return true;
    }
    return false;
  }

  /**
   * Internal: Notify about selection change
   */
  private notifyChange(): void {
    this.showStatus();

    if (this.options.onSelectionChange && this.selected !== null) {
      this.options.onSelectionChange(this.selected, this.currentIndex);
    }
  }
}
