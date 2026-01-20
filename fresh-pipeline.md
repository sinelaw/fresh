# Rendering Pipeline in Fresh Text Editor
    
The source file is text on disk, in UTF-8 format.

A **piece tree** data structure represents this file. Some of the data may be in memory while the rest is pointed at the disk. The piece tree provides an iterator that walks in linear offset order over nodes of the tree, each pointing at chunks of the data. It also provides indexing of the line numbers so it's relatively cheap to find an offset given a line number (or vice versa). For large files, we don't do this indexing to avoid loading the entire file.

The piece tree doesn't actually store any data. It contains information about where the data is stored, by holding an index into an array of StringBuffers. A StringBuffer contains the memory itself (if loaded into RAM) or offset in the backing file (if not loaded into RAM). Modified / inserted bytes are stored in StringBuffers that can grow in size. This allows to reduce memory allocations by writing edits consequetively per edited region (in theory we could make it a single linear memory region but Fresh doesn't currently do that).

The piece tree and its accompanying StringBuffer vector are maintained by TextBuffer, a struct representing a file being displayed or edited (also tracks line ending format LF/CRLF, version counter for LSP, various flags like read only, large file, etc.) The piece tree by itself never loads data or even knows how to access the data, it accepts information from its caller and is a clean data structure decoupled from IO.

TextBuffers provide a LineIterator which starts at some offset and iterates over lines by iterating over piece tree nodes and lazily loading chunks as it proceeds. It's used below during the rendering process. The lazy loading populates pieces of the TextBuffer from disk so that repeated iteration reuses the loaded data.

Each text buffer can have zero or more viewports. The TextBuffer state is shared by all viewports. Each viewport represents a (possibly visible or hidden) tab in a split view on the screen. Viewports have their own separate state: cursors, scroll state, selections, etc. basically anything that we'd want to store per view rather than per underlying buffer.

As explained below, there are many features that require annotating pieces of the text with some metadata (such as highlighting). These are called markers. Since the text is being edited, the markers are not static - they don't stay in their original offset. To avoid re-calculating highlighting, selection regions, etc. on every single keypress, in Fresh we use an **interval tree** to maintain the marker information. The interval tree provides an API for inserting markers by position, and then later efficiently querying their position by ID (efficiently). Between insert and query you can also feed edits like insertions or text removals, into the interval tree, which efficiently shifts the positions of all affected markers. *Overlays* are built on top of the marker interval tree, and pair start/end markers to represent self-adjusting ranges.

To render a viewport, we start at the top offset (maintained as an absolute byte offset) of the view and iterate over lines in the underlying buffer until we fill up the view area. Unfortunately, text does not map cleanly to screen positions. We need to incoporate styles, highlighting, variable width characters (such as tabs), decorations like LSP inlay hints (type hints) and allow plugins to insert 'virtual text' (such as git blame headers or diff filler lines). To support all these, the flow I've ended up using is:

1. Input source text
2. Tokenizer (Base tokens)
3. View Transformer (Plugins / Virtual Text)
4. Wrapping (Line breaks for width limits)
5. Line Generation (ViewLines)
6. Styling & Rendering (Syntax/Semantic highlighting, Overlays, Selection, Cursor)

*Tokenizer*: The tokenization converts raw input bytes into tokens: LF / CRLF to line break tokens, spaces or tabs into dedicated whitespace tokens, binary (non-text) bytes as binary tokens, and collects contiguous blocks of anything else as text tokens.

*Wrapping*: After tokenization and transformations, we handle edge cases such as very long lines (think huge 1GB json file as a single line) by inserting line break tokens if line length exceeds a safety threshold (or the viewport width if soft wrapping is enabled).

The viewport has room for a known number of lines. When processing input at the start of the pipeline we plan to fill up the number of lines in the viewport, but further along the pipeline we could end up stopping early - for example if line wrapping is enabled or if a plugin injects virtual lines or other decorations that use up vertical space.

*View transformer* is a way for plugins to arbitrarily change the stream of tokens (for example by transforming content or injecting virtual text like headers).

*Line Generation* creates the `ViewLine` structures which contain the bi-directional map: source byte offset <-> visual column offset. Both directions of this mapping are needed: when we move the cursor, the movement is visual so we need to know where in the source bytes each visual location maps to. In the other direction (byte offset -> visual column), we use it to calculate cursor screen positions and handle horizontal scrolling.

For the many different highlights and indicators we extract at the start of the render flow the set of markers that apply to our current viewport range. We store these overlays in an array sorted by position and later reference it while rendering. I'm not sure if that's the best approach but it's to avoid multiple O(log n) lookups per each offset in the viewport.

*Syntax higlighting* is currently re-calculated for every frame, but only using a subset of the full file (current viewport plus some large window of preceding text for syntax context). This is done using syntect which provides highlighting using Textmate-based grammars.

*Reference highlighting* is the feature of showing a highlight over a symbol or word in the text where the cursor is positioned and also all other occurances of the word that are visible in the viewport. This is implemented by registering overlays in the interval tree. If the user edits the buffer, the overlays automatically stay correct, ensuring the highlighting doesn't drift during edits. This way the reference highlight overlays are only invalidated and re-created if the cursor moves to a different word, not on every render frame nor on scrolling etc.

*Semantic highlighting* is an LSP feature - we ask the LSP server to provide highlighting tokens, these get translated to overlays (again, to automatically move with edits efficiently). There are two APIs: full, and range. Full gets the semantic highlighting tokens for the entire document. Range is used for the current viewport only. Full also supports "delta" API where the LSP server only reports what has changed (based on didChange events sent from Fresh to the LSP).
