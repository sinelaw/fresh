# Markdown Compose Mode - Feature Analysis

## Current Status vs Requirements

### ✅ Completed Features

#### 1. View Transform Infrastructure
- **Column guides rendering** (split_rendering.rs:1731-1749)
  - Vertical lines at specified columns
  - Useful for table alignment
  - Dimmed styling

- **TypeScript API types**
  - ViewTokenWire interface
  - LayoutHints interface
  - Proper token kind discriminated unions

#### 2. Markdown Plugin - Basic Structure
- Parser for Markdown syntax (headers, lists, code blocks, etc.)
- Overlay-based styling (colors for headers, code, links, etc.)
- Token-based view transforms
- Soft break detection (paragraphs flow)
- Hard break preservation (headers, lists, explicit breaks)

#### 3. Testing
- 8 e2e tests covering all Markdown features
- Sample document with comprehensive syntax coverage
- All tests passing

### ❌ Issues Identified by User

1. **No visible difference when enabling**
   - Line numbers disappear (correct)
   - But soft breaks not visible
   - Styling colors not showing
   - View transform may not be applied

2. **Duplicate commands**
   - "Markdown: Toggle Compose Mode" (plugin)
   - "Compose Mode: Toggle" (core)
   - Need to clarify relationship

3. **Missing integration**
   - Plugin vs core compose mode interaction unclear
   - May need core compose mode to work
   - Or make plugin fully independent

### 🔍 Missing Critical Features

Based on docs/MARKDOWN.md design:

#### A. Core Rendering Features

1. **Centering and margins** (MANDATORY)
   - Compose mode should center content at `compose_width`
   - Side margins should be tinted (different background)
   - Currently happens in core but may not trigger

2. **Line number hiding** (IMPLEMENTED in core)
   - Compose mode hides line numbers automatically
   - This IS working per user feedback

3. **Wrapping behavior** (CRITICAL)
   - View transform submits tokens with soft breaks
   - Core should wrap at compose_width
   - Need to verify wrapping is applied correctly

#### B. Visual Navigation (NOT STARTED)

1. **Up/Down arrow keys**
   - Should move by visual lines (post-wrap)
   - Currently moves by source lines
   - Important for user experience

2. **Cursor positioning**
   - Must map correctly through view transform
   - Cursor on soft break should feel natural

#### C. Markdown Styling (PARTIALLY DONE)

**What we have:**
- Overlay-based colors for syntax elements
- Headers, code, lists, links styled

**What might be missing:**
- Overlays may not be showing (rendering issue?)
- Need to verify colors are visible in terminal
- May need brighter/more distinct colors

**Future enhancements:**
- Underline modifiers (italic, links)
- Background colors for code blocks
- Dimmed styling for Markdown control chars

#### D. Plugin Architecture Issues

1. **View transform submission**
   - Currently submits on every render_start
   - Should be more selective (viewport changes only?)
   - Performance implications

2. **Split awareness**
   - Plugin should get split_id from events
   - Currently may not know which split is rendering
   - Per-split configuration needed

3. **Command registration**
   - Need just ONE toggle command
   - Either use core compose mode OR plugin-only
   - Current approach: plugin-only with view transforms

### 🎯 Decision Points

#### Option A: Plugin-Only Approach
**Pros:**
- Self-contained Markdown experience
- No dependency on core compose mode
- Can customize all behavior

**Cons:**
- Need to handle centering/margins ourselves
- Need to manage line number hiding
- More complex

**Requirements:**
1. View transform with layout hints (compose_width)
2. Command to toggle (done)
3. May need to control line number visibility via API
4. Verify centering/margins work with view transform alone

#### Option B: Plugin + Core Compose Mode
**Pros:**
- Leverage existing compose infrastructure
- Centering, margins, line numbers handled
- Simpler plugin

**Cons:**
- Need to coordinate with core mode
- User must use "Compose Mode: Toggle" first
- Less integrated feel

**Requirements:**
1. Plugin activates when markdown file + compose mode
2. Remove duplicate toggle command
3. Plugin only does: parsing, styling, view transforms

### 📋 Action Plan - What to Fix NOW

#### Priority 1: Make it visible (User's complaint)

1. **Debug why view transform isn't showing**
   - Add logging to see if transform is submitted
   - Check if tokens are correct format
   - Verify viewport range calculation

2. **Verify styling overlays work**
   - Test if colors show in terminal
   - May need brighter colors
   - Check overlay z-ordering

3. **Test soft breaks actually flow**
   - Create test paragraph
   - Toggle compose mode
   - Verify lines flow together

#### Priority 2: Clean up commands

1. **Decide: Plugin-only OR Core+Plugin**
   - Test if centering works with view transform alone
   - If yes: plugin-only
   - If no: integrate with core

2. **Remove duplicate command**
   - Either register only plugin command
   - Or hook into core command

3. **Better status messages**
   - "Markdown Compose: ON" should show what changed
   - Explain soft breaks, styling

#### Priority 3: Visual polish

1. **Verify wrapping at compose_width**
   - Should wrap at 80 columns (default)
   - Test with long lines

2. **Test cursor mapping**
   - Move cursor through soft breaks
   - Verify it feels natural

3. **Improve colors**
   - Test in different terminals
   - May need terminal theme awareness

### 📊 Testing Checklist

Before declaring "done":

- [ ] Open markdown file
- [ ] Toggle compose mode (single command)
- [ ] See visible difference:
  - [ ] Line numbers hidden
  - [ ] Content centered
  - [ ] Paragraph lines flow together
  - [ ] Headers appear styled (color/underline)
  - [ ] Code blocks highlighted
  - [ ] Lists styled
  - [ ] Links styled
- [ ] Cursor navigation works naturally
- [ ] Editing preserves source
- [ ] Toggling off returns to source view
- [ ] No duplicate commands in palette

### 🔧 Technical Debugging

#### Check 1: Is view transform being submitted?
```typescript
// Add logging in processBuffer
editor.debug(`Submitting view transform: ${viewTokens.length} tokens`);
```

#### Check 2: Are tokens correct format?
```typescript
// Log first few tokens
editor.debug(`First token: ${JSON.stringify(viewTokens[0])}`);
```

#### Check 3: Is viewport info correct?
```typescript
// Log viewport calculation
editor.debug(`Viewport: ${viewportStart} - ${viewportEnd}`);
```

#### Check 4: Do overlays show?
```typescript
// After adding overlays
editor.debug(`Added ${tokens.length} overlays with prefix md:`);
```

### 🚀 Next Steps

1. Add debug logging to identify why no visible difference
2. Test in fresh terminal session
3. Verify each feature independently:
   - Overlays alone (disable view transform)
   - View transform alone (disable overlays)
   - Both together
4. Once working, polish and document
5. Add visual navigation
6. Create demo video/screenshot

### 💡 Future Enhancements (Post-MVP)

- Table rendering with column guides
- Image link indicators
- Footnote rendering
- Math equation rendering (KaTeX-style)
- Mermaid diagram hints
- Auto-table of contents
- Live preview split
- Export to HTML/PDF
