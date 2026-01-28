# Remote Filesystem Optimization Plan

This document outlines strategies to improve memory efficiency and performance when handling large files over remote SSH connections.

## Current Limitations

The current implementation of `RemoteFileSystem` and the Python `agent.py` suffer from $O(FileSize)$ memory consumption during several key operations:

1.  **Full-File Writes:** The `write` and `sudo_write` commands require the entire file content to be sent as a single JSON message. This causes RAM spikes in both the Rust editor and the Python agent.
2.  **Lack of Streaming:** The `RemoteFileWriter` buffers all data locally in a `Vec<u8>` and only transmits it upon `sync_all()`.
3.  **Inefficient Appends/Truncations:** Operations like `open_file_for_append` and `set_file_length` are implemented by downloading the entire file, modifying it in memory, and re-uploading it.
4.  **PieceTree Flattening:** Saving a document currently requires "flattening" the `PieceTree` into a contiguous buffer, nullifying the benefits of lazy loading during the save process.

## Proposed Optimizations

### 1. Range-Based Patching (Delta Saving)
Instead of replacing the entire file, the protocol should support structured updates.
- **Mechanism:** The editor sends a sequence of instructions: `Copy(src_offset, len)` and `Insert(data)`.
- **Implementation:** The agent creates a new file by copying unmodified regions from the existing local file and inserting new data provided by the editor.
- **Benefit:** Reduces network traffic and RAM usage to $O(Edits)$ rather than $O(FileSize)$.

### 2. Chunked Streaming Writes
Support multi-part uploads to avoid massive single allocations.
- **Mechanism:** Introduce `open_write_session(path)` and `write_chunk(session_id, data)` commands.
- **Implementation:** The Rust `RemoteFileWriter` flushes its internal buffer to the agent whenever a threshold (e.g., 64KB) is reached.
- **Benefit:** Constant RAM overhead regardless of the destination file size.

### 3. Server-Side Atomic Operations
Move logic that doesn't require editor intervention to the agent.
- **Commands:** Add native `append(path, data)` and `truncate(path, length)` to the Python agent.
- **Benefit:** Simple operations on large files (like log appends) happen instantly without any data transfer besides the new content.

### 4. Remote "Bake" (PieceTree Reconstruction)
Directly integrate the `PieceTree` logic with the remote protocol.
- **Mechanism:** The editor sends a "Bake" recipe to the agent.
    - For `BufferLocation::Stored`: The agent reads directly from the local file on the remote disk.
    - For `BufferLocation::Added`: The editor sends the modified content.
- **Benefit:** This is the most efficient form of saving. The editor never needs to hold a contiguous copy of the file. The remote host handles the reconstruction locally.

## Alternative Approaches

While the proposed optimizations focus on custom protocol extensions, several alternative architectures could achieve similar goals:

### 1. External Delta Tools (rsync algorithm)
Rather than implementing a custom "Bake" or "Patch" logic, the system could utilize a block-level delta algorithm (similar to rsync).
- **Pros:** Proven, highly efficient at finding moved blocks or small changes in large files without explicitly tracking edits.
- **Cons:** Adds significant complexity to the agent; might be redundant since the editor's `PieceTree` already tracks exact change locations.

### 2. Content-Addressable Storage (CAS)
Adopt a Git-like approach where file content is managed as a collection of hashed blobs.
- **Pros:** Built-in deduplication. If the same line or block exists in multiple files (or multiple versions of the same file), it is only stored/transmitted once.
- **Cons:** Significant architectural complexity; requires a local "blob store" on the remote host to manage fragments.

### 3. Sparse File Writing
Utilize filesystem-level sparse file support or random-access writes if the protocol can be extended to support "Write at Offset".
- **Pros:** Very simple to implement if changes are strictly overwrites or extensions.
- **Cons:** Does not handle insertions or deletions in the middle of a file efficiently (which requires shifting all subsequent data).

## Trade-offs and Considerations

| Approach | Implementation Complexity | RAM Efficiency | Network Efficiency |
| :--- | :--- | :--- | :--- |
| **Chunked Streaming** | Medium | **High** | Medium |
| **Remote Bake** | High | **High** | **High** |
| **Delta Algorithm** | High | **High** | **High** |
| **Sparse Writes** | Low | Medium | Medium |
