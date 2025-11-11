# LSP Implementation Guide for Text Editor Authors

This guide provides a technical overview for implementing Language Server Protocol (LSP) support in a generic text editor. It draws insights from the `lsp-mode` implementation in Emacs, but the concepts are applicable to any editor.

## Core Concepts

### Client-Server Communication

The LSP operates on a client-server model, where the text editor is the client and the language server is the server. Communication between the two is done via JSON-RPC 2.0.

- **Requests:** The client sends requests to the server to perform operations like asking for completions or finding definitions. Each request has a unique ID.
- **Responses:** The server sends a response for each request. The response contains the result of the operation and the ID of the original request.
- **Notifications:** Both the client and the server can send notifications to each other. Notifications do not have IDs and do not require a response.

All communication happens over a transport layer, which can be standard I/O streams, TCP sockets, or Web Sockets.

### Buffer Management

A crucial aspect of LSP is keeping the server's representation of a file in sync with the editor's buffer.

- **Saved vs. Unsaved Buffers:** The LSP server doesn't directly access the file system for buffer content. The editor is responsible for sending the buffer's content to the server. This is true for both saved and unsaved buffers.
- **State Management:** The editor must inform the server when a file is opened, changed, saved, or closed. This is done through a series of `textDocument` notifications.
- **Document Versioning:** The editor must maintain a version number for each buffer. This version number is incremented with each change and sent to the server. This allows the server to correctly order and apply changes.

### Capabilities

The LSP is designed to be extensible. Not all language servers support all features, and not all clients can handle all features. Capabilities are used to negotiate the set of supported features between the client and the server.

- **Client Capabilities:** During initialization, the client sends its capabilities to the server. This tells the server what features the client supports.
- **Server Capabilities:** The server responds with its own capabilities, indicating which features it supports.
- **Dynamic Registration:** Some capabilities can be registered and unregistered dynamically during the editor's lifecycle. This allows servers to enable or disable features based on the context, such as the project configuration.

## Feature Implementation Guide

This section details the implementation of common LSP features.

### Initialization

The first communication between the client and server is the initialization handshake.

1.  **`initialize` Request:** The client sends an `initialize` request to the server. This request contains the client's capabilities, the root URI of the project, and other initialization options.

2.  **`initialize` Response:** The server responds with its capabilities. The client should store these capabilities and use them to determine which features to enable.

3.  **`initialized` Notification:** After the client has received and processed the server's capabilities, it must send an `initialized` notification to the server. This signals that the initialization is complete, and the server can start sending notifications to the client.

### Document Synchronization

Document synchronization is the process of keeping the server's view of a file consistent with the editor's buffer. This is achieved through a series of notifications.

#### `textDocument/didOpen`

- **When:** Sent when a file is first opened in the editor.
- **Content:** The notification must include the file's URI, language ID, version number (initially 1), and the full text of the buffer.

#### `textDocument/didChange`

- **When:** Sent when a buffer is modified.
- **Content:** The LSP supports two modes of synchronization:
    - **Full:** The entire content of the buffer is sent with each change. This is simpler to implement but can be inefficient for large files.
    - **Incremental (Implemented):** Only the changed parts of the buffer are sent. Each `Event::Insert` and `Event::Delete` generates an incremental update with:
        - A `Range` specifying the affected text positions (line and UTF-16 character offsets)
        - The replacement text (or empty string for deletions)
        - For insertions, the range has start == end (zero-width at insertion point)
- **Versioning:** The version number of the document must be incremented and included in the notification.

#### `textDocument/didSave`

- **When:** Sent when a buffer is saved to disk.
- **Content:** The notification includes the URI of the document. Some servers also support sending the full text of the buffer on save.

#### `textDocument/didClose`

- **When:** Sent when a file is closed in the editor.
- **Content:** The notification includes the URI of the document. After this notification, the server will no longer consider the file to be open.

### Completions

LSP provides rich, context-aware completions.

- **`textDocument/completion`:**
    - **When:** The client requests completions at a specific cursor position.
    - **Content:** The request includes the document URI and the position.
    - **Response:** The server returns a list of `CompletionItem` objects. Each item can include a label, kind (e.g., function, variable), detail, and documentation.
- **`completionItem/resolve`:**
    - **When:** To provide more detailed information about a completion item, the client can send a `completionItem/resolve` request.
    - **Content:** The request includes the `CompletionItem` to resolve.
    - **Response:** The server returns the resolved `CompletionItem` with additional details like documentation or an `additionalTextEdits` property.

### Diagnostics

Diagnostics are errors and warnings that the server detects in the code.

- **`textDocument/publishDiagnostics`:**
    - **When:** The server sends this notification to the client whenever it has new diagnostic information.
    - **Content:** The notification contains the URI of the document and a list of `Diagnostic` objects. Each diagnostic has a range, a severity (error, warning, etc.), and a message.
    - **Client-side:** The client is responsible for displaying these diagnostics to the user, for example, by highlighting the corresponding code in the editor.

### Navigation

LSP provides several features for navigating code.

- **`textDocument/definition`:**
    - **When:** The user wants to jump to the definition of a symbol.
    - **Response:** The server returns a `Location` or a list of `Location` objects, each containing a URI and a range.
- **`textDocument/implementation`:**
    - **When:** The user wants to find the implementations of an interface or abstract class.
    - **Response:** Similar to `textDocument/definition`, the server returns a list of `Location` objects.
- **`textDocument/references`:**
    - **When:** The user wants to find all references to a symbol.
    - **Response:** The server returns a list of `Location` objects.

### Hover

The hover feature displays information about the symbol under the cursor.

- **`textDocument/hover`:**
    - **When:** The user hovers the mouse over a symbol or requests information about it.
    - **Response:** The server returns a `Hover` object, which contains the information to be displayed. The content can be plain text or Markdown.

### Signature Help

Signature help provides information about the parameters of a function or method call.

- **`textDocument/signatureHelp`:**
    - **When:** The user is typing a function call.
    - **Response:** The server returns a `SignatureHelp` object, which contains a list of signatures and information about the active parameter.

### Code Actions

Code actions are refactorings or quick fixes that the server can perform.

- **`textDocument/codeAction`:**
    - **When:** The user requests code actions for a specific range in the document.
    - **Response:** The server returns a list of `CodeAction` objects. Each code action has a title and an `edit` property, which is a `WorkspaceEdit` that describes the changes to be made.

### Formatting

LSP can be used to format code.

- **`textDocument/formatting`:**
    - **When:** The user wants to format the entire document.
    - **Response:** The server returns a list of `TextEdit` objects that describe the formatting changes.
- **`textDocument/rangeFormatting`:**
    - **When:** The user wants to format a specific range in the document.
    - **Response:** Similar to `textDocument/formatting`, the server returns a list of `TextEdit` objects.

### Semantic Tokens



Semantic tokens provide more detailed and accurate syntax highlighting.



- **`textDocument/semanticTokens/full`:**

    - **When:** The client requests semantic tokens for the entire document.

    - **Response:** The server returns a `SemanticTokens` object, which contains a list of tokens and their types.

- **`textDocument/semanticTokens/range`:**

    - **When:** The client requests semantic tokens for a specific range in the document.

    - **Response:** Similar to `textDocument/semanticTokens/full`, the server returns a `SemanticTokens` object.



## Pitfalls and Edge Cases



Implementing LSP support can have some tricky aspects. Here are some common pitfalls and edge cases to consider:



- **Large Files and Performance:** Incremental synchronization is now implemented to avoid sending the full content of large files on every change. Each edit event (insert/delete) sends only the affected range and replacement text, which significantly improves performance. Additionally, some LSP features, like folding ranges or document symbols, can be slow on large files. The client should consider debouncing requests or using other strategies to avoid blocking the UI.



- **Asynchronous Responses:** All communication with the language server is asynchronous. The client must be able to handle responses that arrive out of order or are delayed. It's also important to handle timeouts and errors gracefully.



- **Server-Specific Quirks:** While the LSP provides a standard, some servers have their own quirks or extensions. It's important to test with different language servers and be prepared to handle their specific behaviors. The `initializationOptions` in the `initialize` request can be used to configure server-specific settings.



- **Error Handling:** The client should be robust to errors from the server. This includes JSON-RPC errors, as well as errors in the content of responses. The client should log errors and, when possible, recover gracefully without crashing.



- **Buffer and URI management**: The client must be careful to always use the correct URI for a buffer. This is especially important when dealing with symlinks or remote files. The client should have a consistent way of converting file paths to URIs and back.



- **Cancellation:** The client should be able to cancel long-running requests. The LSP provides a `$/cancelRequest` notification for this purpose. This is important for features like completion, where the user might type more characters before the server has responded to the first request.
