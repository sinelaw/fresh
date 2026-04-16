Padding line 01 — filler content so there is enough room to scroll up.
Padding line 02 — filler content so there is enough room to scroll up.
Padding line 03 — filler content so there is enough room to scroll up.
Padding line 04 — filler content so there is enough room to scroll up.
Padding line 05 — filler content so there is enough room to scroll up.
Padding line 06 — filler content so there is enough room to scroll up.
Padding line 07 — filler content so there is enough room to scroll up.
Padding line 08 — filler content so there is enough room to scroll up.
Padding line 09 — filler content so there is enough room to scroll up.
Padding line 10 — filler content so there is enough room to scroll up.
Padding line 11 — filler content so there is enough room to scroll up.
Padding line 12 — filler content so there is enough room to scroll up.
Padding line 13 — filler content so there is enough room to scroll up.
Padding line 14 — filler content so there is enough room to scroll up.
Padding line 15 — filler content so there is enough room to scroll up.
Padding line 16 — filler content so there is enough room to scroll up.
Padding line 17 — filler content so there is enough room to scroll up.
Padding line 18 — filler content so there is enough room to scroll up.
Padding line 19 — filler content so there is enough room to scroll up.
Padding line 20 — filler content so there is enough room to scroll up.

Text files come in various encodings: UTF-8 or ASCII are commonly used, but we also support others like UTF-16, Windows-1250/1251/1252, GBK and GBK18030, ShiftJis, and others. In each of these systems, byte values have different meanings and there are different rules (sometimes bytes are control bytes that affect the meaning of other bytes). For simplicity, Fresh decodes all files at the low level TextBuffer layer, and stores all buffer data as UTF-8.

Due to the fact that some bytes affect the meaning of future bytes, not all encodings can be partially loaded (for lazy large file chunking). Some encodings can be easily read even from the middle of a file - these are called "resynchronizable", because even if you've lost the prefix of the stream you can still find a byte that unambiguously resets the encoder state (in ASCII every byte is independent, in unicode there is a well-defined way to find an unambiguous reset though it may take a few bytes to reach it). Other encodings are not resynchornizable (GB18030, GBK, ShiftJis, EucKr) which means: lazy chunk loading is not possible for these files because we must read ALL the data from the beginning of the file to know how to interpret a byte in the middle.
