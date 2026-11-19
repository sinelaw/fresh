# Symbols Nerd Font Mono

`SymbolsNerdFontMono-Regular.woff2` is the web UI's font for private-use
code points (U+E000–U+F8FF, U+F0000–U+10FFFD): the Nerd Font glyphs that file
icons and a theme's `chrome.nerd_font` glyph set put into the editor's cells.
The web bridge serves it at `/fonts/symbols-nerd-font-mono-3.5.1.woff2`; the
stylesheet restricts it to those ranges with `unicode-range`, so it draws every
private-use glyph and nothing else.

## Provenance

- Upstream: Nerd Fonts v3.5.1, `NerdFontsSymbolsOnly.zip`
  (<https://github.com/ryanoasis/nerd-fonts/releases/download/v3.5.1/NerdFontsSymbolsOnly.zip>,
  SHA-256 `fdca3682534f6f65e1ccb2345b0362ccf67d9b8eca7c8025330946e93e2473bc`),
  file `SymbolsNerdFontMono-Regular.ttf`.
- Converted to WOFF2 unchanged (same glyphs and character map, 10,624 code
  points) with fontTools:

  ```python
  from fontTools.ttLib import TTFont
  f = TTFont("SymbolsNerdFontMono-Regular.ttf")
  f.flavor = "woff2"
  f.save("SymbolsNerdFontMono-Regular.woff2")
  ```

## License

The font as a whole is distributed by the Nerd Fonts project under the MIT
License:

```
The MIT License (MIT)

Copyright (c) 2014 Ryan L McIntyre

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

The icon sets inside keep their upstream licenses (as listed in the Nerd Fonts
v3.5.1 release notes for this font):

| Icon set               | Upstream                                              | Version         | License                         |
|------------------------|-------------------------------------------------------|-----------------|---------------------------------|
| Codicons               | https://github.com/microsoft/vscode-codicons          | 0.0.45          | CC BY 4.0                       |
| Devicons               | https://github.com/devicons/devicon                   | 2.17.0          | MIT                             |
| extraglyphs            | https://github.com/source-foundry/Hack                | -               | MIT                             |
| Font Awesome           | https://github.com/FortAwesome/Font-Awesome           | 6.5.1           | CC BY 4.0                       |
| Font Awesome Extension | https://github.com/AndreLZGava/font-awesome-extension | 0.0.3           | MIT                             |
| Font Logos             | https://github.com/lukas-w/font-logos                 | 1.3.0           | The Unlicense (public domain)   |
| MaterialDesign         | https://github.com/Templarian/MaterialDesign-Font     | Oct 6, 2022     | Apache 2.0                      |
| Octicons               | https://github.com/primer/octicons                    | 18.3.0          | MIT                             |
| Seti and original      | https://github.com/jesseweed/seti-ui                  | 0.8.1           | MIT                             |
| Pomicons               | https://github.com/gabrielelana/pomicons              | 1.001           | OFL 1.1, Reserved Font Name     |
| Powerline Extra        | https://github.com/ryanoasis/powerline-extra-symbols  | 1.200           | MIT                             |
| Powerline Symbols      | https://github.com/powerline/powerline                | 1.000 (ca 2013) | MIT                             |
| Power Symbols IEC      | https://github.com/jloughry/Unicode                   | Feb 2015        | MIT                             |
| Weather Icons          | https://github.com/erikflowers/weather-icons          | 2.0.10 (1.100)  | OFL 1.1                         |

- CC BY 4.0 (<https://creativecommons.org/licenses/by/4.0/legalcode>; the
  license allows giving its URI in place of a copy). Attribution: Codicons
  © Microsoft Corporation; Font Awesome Free © Fonticons, Inc. The glyphs are
  redistributed unmodified, converted to WOFF2 only.
- OFL 1.1: the license terms are in `LICENSE-OFL.txt` (that file's header
  names JetBrains Mono, the other font in this directory; the terms are the
  same). For the icon sets in this font under it:
  - Pomicons: Copyright (c) 2021, Gabriele Lana, with Reserved Font Name
    Pomicons.
  - Weather Icons: icon designs by Lukas Bischoff and Erik Flowers
    (<https://github.com/erikflowers/weather-icons>).
- Apache 2.0 (MaterialDesign, © Pictogrammers / Templarian): the full text is
  in `LICENSE-Apache-2.0.txt`.
- Font Logos: brand icons are trademarks of their respective owners and
  should only be used to represent the company or product they refer to.
