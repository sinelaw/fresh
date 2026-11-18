# Text Encoding

Fresh automatically detects and handles various text encodings.

## How It Works

The in-memory encoding is always UTF-8. Files are converted to UTF-8 when loaded and converted back to the original encoding when saved. The encoding shown in the status bar indicates the **on-disk encoding**:

- In-memory: always UTF-8 (full Unicode support)
- On-disk: the encoding shown in the status bar
- Changing the status bar encoding changes how the file will be saved

## Supported Encodings

- **UTF-8** (default), **UTF-8 BOM**, **ASCII**
- **UTF-16 LE/BE** (with BOM detection)
- **Latin-1** (ISO-8859-1)
- **Windows-1252**, **Windows-1250**, **Windows-1251**
- **GBK**, **GB18030** (Chinese)
- **Shift-JIS** (Japanese)
- **EUC-KR** (Korean)

## Status Bar Indicator

The current encoding is shown in the status bar. Click it to change the encoding the file is saved in (the same as **Set Encoding** in the command palette); the text already loaded is not re-read.

## Fixing Wrong Encoding Detection

Encoding detection is heuristic — Fresh sniffs byte patterns to guess the on-disk encoding, and short or ambiguous files (especially those without any non-ASCII bytes) can end up tagged incorrectly. If a file opens as garbled text, the fix is to reload it with the right encoding:

1. **Command Palette**: `Ctrl+P` → type "Reload with Encoding"
2. **File Menu**: File → Reload with Encoding...

Cyrillic-script files (Windows-1251) with a mix of uppercase and lowercase letters are detected automatically.

## File Browser Encoding Toggle

When opening files via the file browser (`Ctrl+O`):

- Press `Alt+E` to toggle "Detect Encoding"
- When disabled, you'll be prompted to select an encoding manually

## Large File Confirmation

Large files (over 10 MB) in GBK, GB18030, Shift-JIS or EUC-KR have to be loaded and converted in full, so Fresh asks before loading them. Other encodings open large files lazily, without a prompt.
