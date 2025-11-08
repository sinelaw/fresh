# Language Support

This document outlines the editor's support for various programming languages, including the tools used for syntax highlighting and LSP integration.

## Guiding Principles

1.  **Default Experience:** The editor provides default configurations for syntax highlighting and LSP servers. If the recommended tools are found in the user's `PATH`, they will work automatically.
2.  **User Control:** Users can override default LSP configurations in their `config.json` to use different servers or specify custom paths.
3.  **Clear Feedback:** If a recommended LSP server is not found, the editor will provide a notification with instructions on how to install it.

## Supported Languages

| Language | File Extensions | `tree-sitter` Crate | Recommended LSP Server |
| :--- | :--- | :--- | :--- |
| **Python** | `.py` | `tree-sitter-python` | `pylsp` |
| **JS/TS** | `.js`, `.jsx`, `.ts`, `.tsx` | `tree-sitter-javascript`, `tree-sitter-typescript` | `typescript-language-server` |
| **HTML** | `.html` | `tree-sitter-html` | `vscode-html-languageserver-bin` |
| **CSS** | `.css` | `tree-sitter-css` | `vscode-css-languageserver-bin` |
| **C/C++** | `.c`, `.h`, `.cpp`, `.hpp` | `tree-sitter-c`, `tree-sitter-cpp` | `clangd` |
| **Shell** | `.sh` | `tree-sitter-bash` | `bash-language-server` |
| **Java** | `.java` | `tree-sitter-java` | `jdtls` |
| **Kotlin** | `.kt`, `.kts` | `tree-sitter-kotlin` | `kotlin-language-server` |
| **Go** | `.go` | `tree-sitter-go` | `gopls` |
| **Swift** | `.swift` | `tree-sitter-swift` | `sourcekit-lsp` |
| **Ruby** | `.rb` | `tree-sitter-ruby` | `solargraph` |
| **R** | `.r` | `tree-sitter-r` | `languageserver` (R package) |
| **XML** | `.xml` | `tree-sitter-xml` | `lemminx` |

*Note: Markdown support has been temporarily removed due to `tree-sitter` dependency conflicts. It will be re-evaluated in a future iteration.*

---

## Language-Specific Details

Below are details for each supported language, including installation and configuration of the recommended LSP server.

### Python

*   **LSP Server:** `pylsp`
*   **Installation:**
    ```bash
    pip install python-lsp-server
    ```
*   **Configuration:** The editor will use `pylsp` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### JavaScript / TypeScript

*   **LSP Server:** `typescript-language-server`
*   **Installation:**
    ```bash
    npm install -g typescript-language-server typescript
    ```
*   **Configuration:** The editor will use `typescript-language-server --stdio` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### HTML

*   **LSP Server:** `vscode-html-languageserver-bin`
*   **Installation:**
    ```bash
    npm install -g vscode-html-languageserver-bin
    ```
*   **Configuration:** The editor will use `vscode-html-languageserver-bin --stdio` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### CSS

*   **LSP Server:** `vscode-css-languageserver-bin`
*   **Installation:**
    ```bash
    npm install -g vscode-css-languageserver-bin
    ```
*   **Configuration:** The editor will use `vscode-css-languageserver-bin --stdio` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### C / C++

*   **LSP Server:** `clangd`
*   **Installation:** `clangd` is typically installed with the `llvm` toolchain. Use your system's package manager.
    ```bash
    # Example for Debian/Ubuntu
    sudo apt-get install clangd
    ```
*   **Configuration:** The editor will use `clangd` by default if it is in your `PATH`. A `compile_commands.json` file is required for full project-aware features.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### Shell (Bash)

*   **LSP Server:** `bash-language-server`
*   **Installation:**
    ```bash
    npm install -g bash-language-server
    ```
*   **Configuration:** The editor will use `bash-language-server start` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### Java

*   **LSP Server:** `jdtls`
*   **Installation:** Requires a more complex setup involving downloading the server and creating a wrapper script. Detailed instructions will be provided in the editor's documentation.
*   **Configuration:** Due to the complexity, the user will likely need to configure the path to their `jdtls` wrapper script in `config.json`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*

### Go

*   **LSP Server:** `gopls`
*   **Installation:**
    ```bash
    go install golang.org/x/tools/gopls@latest
    ```
*   **Configuration:** The editor will use `gopls` by default if it is in your `PATH`.
*   **Pros:**
    *   *Placeholder for research*
*   **Cons:**
    *   *Placeholder for research*
*   **Common Issues:**
    *   *Placeholder for research*
