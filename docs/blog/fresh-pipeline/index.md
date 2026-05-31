---
title: "The Architecture of Fresh: Memory-Efficient from the Ground Up"
date: 2026-05-27
description: "A deep dive into the memory-efficient design behind Fresh: piece tree storage, interval trees for markers, and the rendering pipeline."
outline: false
---

# The Architecture of Fresh: Memory-Efficient from the Ground Up

<figure>
  <img src="./showcase.gif" alt="Fresh opening a 400 MB file instantly" />
  <figcaption><em>A 400 MB file being opened instantly.</em></figcaption>
</figure>

I'm tired of every modern tool taking GBs of ram liberally. Fresh - the text editor and IDE - was born of this frustration. I designed it from the ground up to be memory-efficient. Fresh is not a clone of any other text editor - it's a completely new implementation. I started from huge file support, but the design evolved as I added more of the features every text editor (or IDE) is expected to have. It turns out rendering text and allowing users to edit it - with all the extra features - is not so simple! This post walks through how the text rendering flow is built in Fresh.

We'll start by listing a few requirements:

- Everything should be fast and snappy - low latency input handling.
- Keep memory usage low. For huge files - avoid loading them entirely into memory.
- Support many on-disk file formats (text encodings: UTF variants, Windows-* and CJK formats, etc.)
- Syntax highlighting, selection highlighting, current symbol highlighting, etc.
- All the styling and highlighting should move naturally with the text during edits.
- Support plugins, and allow them to customize aspects of rendering:
    + Set arbitrary styles on text ranges
    + Add gutter indicators (icons in the line number column area)
    + Add "virtual text" elements inline in the text or between lines
    + Create completely arbitrary buffers of "virtual content" managed by the plugin"
- No bugs!

These requirements lead to a few design decisions later on. We'll start with the storage layer, which achieves consistently low memory usage, even when loading huge files.

## Storage/Memory Layer: The Piece Tree

A **piece tree** data structure represents contents of a file. Some of the data may be in memory, while the rest is in the file (on disk). The piece tree includes a line-number index for cheap lookup - byte offset to line number, or the other way around - find the byte offset of a given line number (which is important for "go to line" features). For large files, we don't do this indexing automatically to avoid loading the entire file. We can index the lines on request (if the user wants "go to line") by streaming through the file without loading it all into memory at once. The streaming indexing also supports remote files (indexing without fetching the entire file over the network).

The piece tree itself doesn't actually store any data. It contains information about where the data is stored, by holding an index into an array of StringBuffers. A StringBuffer contains the memory itself (if loaded into RAM) or offset in the backing file (if not loaded into RAM). Modified / inserted bytes are stored in StringBuffers that grow in size. These growable buffers allow reducing memory allocations by writing edits consecutively per edited region (in theory we could make it a single linear memory region but Fresh doesn't currently do that).

A simplified version of the code might look like this - the actual code is similar:

```rust
pub enum StringBuffer {
    Loaded {
        data: Vec<u8>,
        line_starts: Option<Vec<usize>>, // positions of newlines
        file_offset: Option<usize>, // if loaded from disk and unmodified
    },
    Unloaded {
        file_offset: usize,
        bytes: usize,
    },
}
```

A single StringBuffer may serve more than one node in the piece tree. The nodes point into an offset within the StringBuffer.

```rust
enum PieceTreeNode {
    Internal {
        left_bytes: usize,          // Total bytes in left subtree
        lines_left: Option<usize>,  // Total newlines in left subtree
        left: Arc<PieceTreeNode>,
        right: Arc<PieceTreeNode>,
    },
    Leaf {
        location: BufferLocation,
        offset: usize,                // Offset within the buffer
        bytes: usize,                 // Number of bytes in this piece
        lines_count: Option<usize>,   // Number of lines in this piece
    },
}
```

The tree structure is what enables efficient lookup & insertion: given a byte offset, we walk down the left/right nodes to reach the `Leaf` that contains the desired data. Inserts and edits are managed by splitting nodes. The tree can be rebalanced to minimize the depth.

The API includes things like:
- `insert`, which inserts bytes by walking from the root down to the leaf where the bytes belong, splits that leaf, and updates all the ancestors back up to the root.
- `offset_to_position` which converts a byte offset to a line number + column number.
- `position_to_offset` which converts a line number + column number to byte offset, and `line_range` which converts line number to byte offset start/end range. Both of these work by walking the tree's nodes by line index instead of by byte offset.
- Iterators from a given offset, etc.

### Efficient Tree Diffs

Having a piece tree made it easy to implement a few features, like fault recovery. To ensure unsaved data is recovered we need to store it often (every few seconds). To make this process quick we save only the non-file-backed buffers to disk which is fast even for huge files. Also diffing the in-memory buffer against the disk / finding modified regions for gutter markers is fast, we look for non-file-back nodes and then compare only those regions to the disk version.

So imagine we want to iterate all unsaved regions in a buffer - pieces of data that were inserted or modified by the user but not yet saved to disk. We can walk the entire tree structure and find leaf nodes that point to StringBuffers that have modifications, but this will be slow if the tree is large. To speed things up, we store two copies of the tree:
1. A "pristine tree", represents the data as it was when first opening the file on disk (this is called `saved_root` in the Fresh source code).
2. A "working tree", which includes any modifications to the file data done in the editor.

To find which parts of a file (imagine a huge file) have been modified, we walk the two trees in tandem and whenever we hit a branch (`Internal`) node that has the exact same left or right branch, we can completely skip those branches and avoid iterating further down. In Fresh this is called a "structural diff".

Note that tree *structure* can change, even when the data itself is unchanged. For example, when loading chunks of data from disk, we update the tree nodes to point at the loaded data in that region instead of pointing at the disk. The tree structure changes even though there was zero change to the underlying data content, and this means we can't simply compare two trees by their structure to see if they have different data (they might have exactly the same data), which boils down to wasteful work.

To make the structural diff robust, we need to keep the pristine tree structure in sync with data loading operations. The pristine reference tree that was created when we first loaded the file, is continuously mutated (replaced, actually) every time we load data from disk to memory to keep it near the same "shape" as the mutable "working" tree. With the shape-tracking updates of both the pristine tree and the working tree, they are likely to have the same structure - except for regions where the data was actually modified. When comparing the two trees we can now spend most of the algorithmic effort on the areas that are likely to contain edits and completely skip everything else.

The structure may still not match 100% in regions where we modified data. A node could have been split and data edited, but the end result could be that the data in memory is actually identical to the one on disk. To complement the structural diff we also compare byte-by-byte regions of the tree which don't match. There's a choice here - for some use cases we could just say that structure mismatches are treated as an real difference (even if the bytes are identical), for example for dumping recovery data we can just dump the region of data even if it *may* be unmodified. For other features like showing accurate diff indicators on the gutter, we would still want to compare the region, byte-by-byte.

### Property Testing for Piece Tree

I'm paranoid about having a data loss or corruption bug, as I should. To sleep better at night I use property testing on the piece tree. These tests generate a set of randomized operations, apply them to the tree, and check that certain invariants are always true. Here are some of them:

- Total byte count as reported by the tree = sum of all insert/delete operations.
- Tree is balanced, to at most some level of imbalance.
- Insert followed by delete in the same range = original data.
- Sum of all piece lengths = total tree length, and same for line numbers

At a higher level I have tests that perform a workload on a tree and an identical on a simple array, and then compares byte-by-byte the final contents as reported by the tree vs. the simple arrary. After all the splitting, balancing, node-iterating and data merging inside the tree shouldn't make a different for the end result and the two should be equal. This shows that our tree is nothing more than an optimization.

All these tests are executed as property tests - the operations are generated and arbitrary, not just a single scenario or a handful of specific scenarios. The tests follow the general pattern of the following example:

1. Generate random operations
2. Apply them on the piece tree and also on a "shadow model", a very simple analog (vector in this case)
3. Compare the values of the tree to the value of the shadow model.

The *observable state* exposed via API should be identical between the tree and the model. Here is a pseudocode example of such a test:

```rust
strategy InitialContent -> Vec<u8>
strategy EditOperations -> List<BufferOp> { Insert(pos, bytes), Delete(pos, len), ... }

proptest roundtrip_preserves_content(
    initial_content in InitialContent,
    ops in EditOperations
) {
    let file_path = create_temp_file(initial_content);

    let mut buffer = TextBuffer::load(file_path);
    let mut shadow_vec = initial_content.clone(); // The "Oracle" source of truth

    for op in ops {
        op.apply_to_buffer(&mut buffer);
        op.apply_to_shadow(&mut shadow_vec);
    }

    let pre_save_content = buffer.read_all();
    assert_eq!(pre_save_content, shadow_vec, "In-memory state diverged!");

    let save_path = get_new_temp_path();
    buffer.save(save_path);

    let reloaded_buffer = TextBuffer::load(save_path);
    let reloaded_content = reloaded_buffer.read_all();

    assert_eq!(reloaded_content, pre_save_content, "Save/Load roundtrip corrupted data!");
}
```

## TextBuffer, the virtual "buffer" layer

The next layer up, built on top of the piece tree, is the TextBuffer. The piece tree and its accompanying StringBuffer vector are owned by TextBuffer. It's a struct representing a single file being displayed or edited (also tracks line ending format LF/CRLF, version counter for LSP, various flags like read only, large file, etc.) The piece tree by itself never loads data, it accepts information from its caller and is a clean data structure decoupled from IO. The TextBuffer ties the IO side-effects with the piece tree, making it easier to test the tree in isolated memory-only property tests.

TextBuffers provide a LineIterator which starts at some offset (using the tree API to efficiently bisect into the correct node) and iterates over lines by iterating over piece tree nodes and lazily loading chunks as it proceeds. It's used in some example described below, during the rendering process. The lazy loading populates pieces of the TextBuffer from disk so that repeated iteration reuses the loaded data. This is one of the cases where a read only operation (just iterating lines) causes the tree to mutate - change structure - to accommodate caching.

Each text buffer can have zero or more viewports. The TextBuffer state is shared by all viewports. Each viewport represents a (possibly visible or hidden) tab in a split view on the screen. Viewports have their own separate state: cursors, scroll state, selections, etc. basically anything we'd want to store per view rather than per underlying buffer.

## Overlay Markers and the Interval Tree

Many editor features require annotating text regions. For example, selection highlighting shows a visual cue aroud the piece of text selected by the user. Error indicators decorate parts of the code that cause compilation errors, etc. These text annotations are called markers. Markers have an ID which is used to look them up in a per-feature table, and an offset in the text. As the text is edited, the markers must shift around. Markers don't stay in their original offset.

To avoid re-calculating the offset of markers (like selection regions) on every single keypress, in Fresh we use an **interval tree**. The interval tree is used to maintain the marker offset as the text moves around. The interval tree provides an API for inserting markers by position, and then later efficiently querying their position by ID. Between insert and query you can also feed edits like insertions or text removals, which efficiently shifts the positions of all affected markers. *Overlays* are built on top of the marker interval tree, and pair start/end markers to represent self-adjusting ranges.

The interval tree has the following structure (adapted from real code):

```rust
struct Marker {
    id: MarkerId,
    interval: Interval, // absolute offset
}

struct Node {
    marker: Marker,  // includes the marker's range in absolute offset
    max_end: u64,    // highest offset of any marker in this subtree
    lazy_delta: i64, // allows quick updates without traversing many nodes

    parent: WeakNodePtr,
    left: NodePtr,
    right: NodePtr,
}

pub struct IntervalTree {
    root: NodePtr,
    next_id: u64,
    /// ID-to-Node map for O(1) lookups
    marker_map: HashMap<MarkerId, Rc<RefCell<Node>>>,
}
```

I'll skip details like height (used for rebalancing). The basic API is:

- Insert/delete markers (by offset)
- Adjust marker offsets to account for text insertion/deletion by position and size.
- Lookup markers in a range of offsets
- Lookup marker by ID

As the user edits their buffer, the "adjust markers" API is used by the editor to shift markers around. Using the `lazy delta` allows doing this efficiently without traversing the marker list / tree on every edit. Upon shifting offsets we find a node where the shift belongs (O(log n) search) and update `lazy_delta`. During this process we push any pre-existing deltas at ancestor nodes down to their immediate children, so we always have a clean path (zero deltas) to the root. This work of pushing deltas down is done in insert/delete markers and other operations that search down the tree.

As you can see, a simple text insert/delete operation normally only searches down the tree (log N) and then updates a single field (`lazy_delta`). It doesn't need to iterate over the entire rest of the document and update all markers.

When rendering text, we need to quickly find which markers are applicable to the currently rendered text range. We seek into the interval tree and start iterating from the viewport start (an absolute offset of the first character that we render on the screen). As we iterate the text, we also iterate through the nodes of the interval tree in offset order (left to right) and collect marker IDs. When we hit a non-zero `lazy_offset` we accumulate it to the current offset so that all affecter markers are reported as having the correct, adjusted offset.

All this allows for efficient storage and querying of arbitrary metadata that shifts around as the text is edited.

## Rendering

To render a viewport, start at the top offset (maintained as an absolute byte offset) of the view and iterate over lines in the underlying buffer until filling up the view area. Unfortunately, text does not map cleanly to screen positions. We need to incoporate line wrapping, styles, highlighting, variable width characters (such as tabs), decorations like LSP inlay hints (type hints) and allow plugins to insert 'virtual text' (such as git blame headers or diff filler lines). To support all these, the flow I've ended up using is:

1. Input source text
2. Tokenizer (Base tokens)
3. View Transformer (Plugins / Virtual Text)
4. Wrapping (Line breaks for width limits)
5. Line Generation (ViewLines)
6. Styling & Rendering (Syntax/Semantic highlighting, Overlays, Selection, Cursor)

*Tokenizer*: converts raw input bytes into tokens: LF / CRLF into line break tokens, spaces or tabs into dedicated whitespace tokens, binary (non-text) bytes as binary tokens. Contiguous blocks of anything else are collected into batched text tokens.

*Wrapping*: After tokenization and transformations, edge cases are handled - such as very long lines (think huge 1GB json file as a single line) by inserting line break tokens if line length exceeds a safety threshold (or the viewport width if soft wrapping is enabled).

The viewport has room for a known number of lines, but the pipeline can't know in advance how many visual rows it will produce. For example if line wrapping is enabled or if a plugin injects virtual lines or other decorations that use up vertical space.

*Line Generation* creates the `ViewLine` structures which contain the bi-directional map: source byte offset <-> visual column offset. Both directions of this mapping are needed: when we move the cursor up one line, the movement is visual so we need to know where in the source bytes each visual location maps to. In the other direction (byte offset -> visual column), we use it to calculate cursor screen positions and handle horizontal scrolling.

For the many different highlights and indicators we extract at the start of the render flow the set of markers that apply to our current viewport range. We store these overlays in an array sorted by position and later reference it while rendering. I'm not sure if that's the best approach but it's to avoid multiple O(log n) lookups per each offset in the viewport.

*Syntax higlighting* is currently re-calculated for every frame, but only using a subset of the full file (current viewport plus some large window of preceding text for syntax context). This is done using syntect which provides highlighting using Textmate-based grammars.

For normal files, we parse the entire file with the syntax highlighter, and store two things:

1. All the span information (highlight category of every symbol in the file), stored via markers in an intervale tree.
2. A set of cached parser snapshots, once every 256 bytes. Each snapshot is the parser's state at that offset.

When the user edits the buffer, we update the interval tree to shift the markers around, and then we lookup the nearest previous parser snapshot. We then re-run the parser starting at the snapshot and continue parsing and updating the parser snapshots every 256 bytes. If we hit a snapshot that is identical to the already cached parser state at that byte offset, we can stop parsing: it means the parser has converged on an identical state as before.

For large files, we don't parse the entire file, only a region surrounding the viewport. This partial parsing allows instantaneous loading and display of large files with capped memory usage and low latency.

*Reference highlighting* is the feature of showing a highlight over a symbol or word in the text where the cursor is positioned and also all other occurrences of the word that are visible in the viewport. This is implemented by registering overlays in the interval tree. If the user edits the buffer, the overlays automatically stay correct, ensuring the highlighting doesn't drift during edits. This way the reference highlight overlays are only invalidated and re-created if the cursor moves to a different word, not on every render frame nor on scrolling etc.

*Semantic highlighting* is an LSP feature - we ask the LSP server to provide highlighting tokens, these get translated to overlays (again, to automatically move with edits efficiently). There are two APIs: full, and range. Full gets the semantic highlighting tokens for the entire document. Range is used for the current viewport only. Full also supports "delta" API where the LSP server only reports what has changed (based on didChange events sent from Fresh to the LSP).

### Dead ends

*View transformer* is a way for plugins to arbitrarily change the stream of tokens (for example by transforming content or injecting virtual text like headers). Honestly, I'm not sure I need it - the idea was to allow plugins to completely rewrite the token stream that gets rendered. All the use cases I had in mind are better served by other mechanisms: markdown preview, for example, uses "omit" overlays tied to specific positions in the stream, to remove markup. I also don't like the view transform because:
1. It breaks the "everything is done incrementally" model, because it's monolithic - processes the entire viewport at every frame render.
2. It introduces potential nondeterminism into the render flow - a plugin may return different transformation result for the same input at different times.

I'll probably remove view transforms unless I find a unique use case that can't be solved in another way aside from allowing a plugin to fully transform the incoming token stream.

## Renderer Output

The renderer pipeline constructs a set of ViewLines, which are objects describing visual rows on the screen. It simultaneously iterates over the various marker trees (interval trees) in lockstep and maintains a list of currently active markers, which enter and leave this list as the iteration over the source bytes progresses. The ViewLines are composed of text spans with their final calculated decorations. The text spans also take care of accumulating unicode characters into graphemes, which are sets of characters that must be rendered in a single overlapping position on the screen (for example in Thai). Also per visual line we calculate the gutter info (various icons like "line changed in git" indicators) + line numbers.

All of these visual lines are emitted in a single `LineRenderOutput` struct.

The next step passes this calculated rendered output to the drawing functions, which convert it to a ratatui input and sends it off to ratatui (the excellent TUI rendering library used by Fresh).

## Client/Server Sessions

Fresh supports running a server-mode process that retains a session, which you can detach or reattach from, using another process acting as a client. This is useful for sending "open file" commands to an already open Fresh process from another program via a cli command, such as a coding agent, or a git command that needs an editor, or a file manager (like Yazi). Another use case is reconnecting to a session you started earlier on a remote machine, if you don't want to use a terminal multiplexer like tmux (I myself use tmux extensively, but not everybody likes it). emacsclient is an example of the same feature in another text editor.

Clients in Fresh are very thin. They send terminal events to the server, and receive from the server the raw rendering output - pre-rendered ANSI bytes. All the client needs to do for rendering is to set the terminal mode to raw, and pipe bytes from the server to the terminal.

## Summary

Memory efficiency and low latency drive the architecture of Fresh to **only do as much work as needed**:

- The piece tree with optional lazy-loading support enables huge files to be loaded instantly and with minimal memory overhead. Small (i.e. normal) files load everything to memory and benefit from the advantages.
- The rendering pipeline only walks / evaluates the parts of the tree that are required for filling the current viewport. Never the entire buffer.
- Syntax highlighting uses caching that re-synchronizes to a near checkpoint after buffer edits, and makes an effort to avoid whole-buffer reparsing. For huge files, syntax highlighting only parses a small area around the current viewport window.
- Interval trees make it efficient to lookup / iterate / mutate sub-ranges of metadata annotations while keeping them aligned with the text by efficiently adjusting to offset shifts.

There are many other aspects the design of Fresh that may be interesting to blog about (generic settings editor, split pane system, embedded terminals and scrollback, prompts and command palettes). These all rely on the core storage and rendering pipeline.
