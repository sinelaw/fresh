# Text Encoding

Fresh automatically detects and handles various text encodings.

## How It Works

The in-memory encoding is always UTF-8. Files are converted to UTF-8 when loaded and converted back to the original encoding when saved. The encoding shown in the status bar indicates the **on-disk encoding**:

- In-memory: always UTF-8 (full Unicode support)
- On-disk: the encoding shown in the status bar
- Changing the status bar encoding changes how the file will be saved

## Supported Encodings

- **UTF-8** (default)
- **UTF-16 LE/BE** (with BOM detection)
- **Latin-1** (ISO-8859-1)
- **Windows-1252**, **Windows-1250**
- **GBK**, **GB18030** (Chinese)
- **Shift-JIS** (Japanese)
- **EUC-KR** (Korean)

## Status Bar Indicator

The current encoding is shown in the status bar. Click it to change the encoding.

## Reload with Different Encoding

If a file is detected incorrectly, reload it with a specific encoding:

1. **Command Palette**: `Ctrl+P` → type "Reload with Encoding"
2. **File Menu**: File → Reload with Encoding...
3. **Status Bar**: Click the encoding indicator

Select the correct encoding from the list to reload the file.

## File Browser Encoding Toggle

When opening files via the file browser (`Ctrl+O`):

- Press `Alt+E` to toggle "Detect Encoding"
- When disabled, you'll be prompted to select an encoding manually

## Large File Confirmation

For large files (>10MB) with non-UTF-8 encodings, Fresh shows a confirmation prompt before loading since encoding conversion cannot be incrementally re-synchronized.
