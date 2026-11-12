/**
 * LaTeX math as Unicode text, for drawing a formula in a terminal: Greek
 * letters and symbols by name, superscripts and subscripts where Unicode has
 * them, and fractions and roots spelled out inline.
 *
 * Pure functions with no editor calls, so they can be unit-tested
 * (`tests/latex.test.ts`).
 */

const GREEK_MAP: Record<string, string> = {
  alpha: "α", beta: "β", gamma: "γ", delta: "δ", epsilon: "ε", varepsilon: "ε",
  zeta: "ζ", eta: "η", theta: "θ", vartheta: "ϑ", iota: "ι", kappa: "κ",
  lambda: "λ", mu: "μ", nu: "ν", xi: "ξ", pi: "π", varpi: "ϖ",
  rho: "ρ", varrho: "ϱ", sigma: "σ", varsigma: "ς", tau: "τ", upsilon: "υ",
  phi: "φ", varphi: "ϕ", chi: "χ", psi: "ψ", omega: "ω",
  Gamma: "Γ", Delta: "Δ", Theta: "Θ", Lambda: "Λ", Xi: "Ξ",
  Pi: "Π", Sigma: "Σ", Upsilon: "Υ", Phi: "Φ", Psi: "Ψ", Omega: "Ω",
};

const SYMBOL_MAP: Record<string, string> = {
  times: "×", cdot: "·", div: "÷", pm: "±", mp: "∓",
  leq: "≤", le: "≤", geq: "≥", ge: "≥", neq: "≠", ne: "≠",
  approx: "≈", equiv: "≡", sim: "∼", simeq: "≃", cong: "≅", propto: "∝",
  ll: "≪", gg: "≫",
  infty: "∞", partial: "∂", nabla: "∇", hbar: "ℏ", ell: "ℓ",
  sum: "∑", prod: "∏", coprod: "∐", int: "∫", iint: "∬", iiint: "∭", oint: "∮",
  in: "∈", notin: "∉", ni: "∋", subset: "⊂", supset: "⊃",
  subseteq: "⊆", supseteq: "⊇", cup: "∪", cap: "∩", setminus: "∖", emptyset: "∅",
  forall: "∀", exists: "∃", nexists: "∄",
  land: "∧", lor: "∨", neg: "¬", wedge: "∧", vee: "∨",
  to: "→", rightarrow: "→", gets: "←", leftarrow: "←", leftrightarrow: "↔",
  Rightarrow: "⇒", Leftarrow: "⇐", Leftrightarrow: "⇔", implies: "⇒", iff: "⇔",
  mapsto: "↦", uparrow: "↑", downarrow: "↓",
  angle: "∠", perp: "⊥", circ: "∘", bullet: "•", star: "⋆", ast: "∗",
  cdots: "⋯", ldots: "…", vdots: "⋮", ddots: "⋱",
  prime: "′", dag: "†", ddag: "‡", aleph: "ℵ", Re: "ℜ", Im: "ℑ",
  oplus: "⊕", otimes: "⊗", odot: "⊙",
  parallel: "∥", Vert: "‖", vert: "|",
  langle: "⟨", rangle: "⟩", lfloor: "⌊", rfloor: "⌋", lceil: "⌈", rceil: "⌉",
  quad: "  ", qquad: "    ",
  dots: "…", therefore: "∴", because: "∵", degree: "°", lnot: "¬",
  mid: "|", colon: ":", lbrace: "{", rbrace: "}", lvert: "|", rvert: "|",
  longrightarrow: "⟶", longleftarrow: "⟵", Longrightarrow: "⟹", Longleftarrow: "⟸",
  longmapsto: "⟼", hookrightarrow: "↪", rightleftharpoons: "⇌",
  top: "⊤", bot: "⊥", vdash: "⊢", models: "⊨", triangle: "△", square: "□",
  checkmark: "✓", sqcup: "⊔", sqcap: "⊓", wp: "℘", varnothing: "∅",
  omicron: "ο", varkappa: "ϰ", digamma: "ϝ",
};

/** Symbols that read as part of the term after them, like a letter does. */
const LETTER_LIKE = new Set(["partial", "nabla", "hbar", "ell", "aleph", "wp", "Re", "Im"]);

/** Letters Unicode gives their own code point outside the math-alphanumeric block. */
const DOUBLE_STRUCK_HOLES: Record<string, string> = {
  C: "ℂ", H: "ℍ", N: "ℕ", P: "ℙ", Q: "ℚ", R: "ℝ", Z: "ℤ",
};
const SCRIPT_HOLES: Record<string, string> = {
  B: "ℬ", E: "ℰ", F: "ℱ", H: "ℋ", I: "ℐ", L: "ℒ", M: "ℳ", R: "ℛ",
  e: "ℯ", g: "ℊ", o: "ℴ",
};

/** `\mathbb` and `\mathcal` letters: ℝ, ℤ, 𝔽, 𝒪 … Anything else is left as it is. */
function mathAlphabet(str: string, font: "mathbb" | "mathcal"): string {
  let res = "";
  for (const ch of str) {
    const holes = font === "mathbb" ? DOUBLE_STRUCK_HOLES : SCRIPT_HOLES;
    if (holes[ch]) { res += holes[ch]; continue; }
    const upper = font === "mathbb" ? 0x1d538 : 0x1d49c;
    const lower = font === "mathbb" ? 0x1d552 : 0x1d4b6;
    if (ch >= "A" && ch <= "Z") res += String.fromCodePoint(upper + ch.charCodeAt(0) - 65);
    else if (ch >= "a" && ch <= "z") res += String.fromCodePoint(lower + ch.charCodeAt(0) - 97);
    else if (font === "mathbb" && ch >= "0" && ch <= "9") res += String.fromCodePoint(0x1d7d8 + ch.charCodeAt(0) - 48);
    else res += ch;
  }
  return res;
}

/** Accents drawn as a combining mark after each character of their argument. */
const ACCENT_MAP: Record<string, string> = {
  hat: "\u0302", widehat: "\u0302", bar: "\u0304", overline: "\u0305", vec: "\u20d7",
  dot: "\u0307", ddot: "\u0308", tilde: "\u0303", widetilde: "\u0303", check: "\u030c",
  acute: "\u0301", grave: "\u0300", breve: "\u0306", underline: "\u0332",
};

const SUPERSCRIPT_MAP: Record<string, string> = {
  "0": "⁰", "1": "¹", "2": "²", "3": "³", "4": "⁴",
  "5": "⁵", "6": "⁶", "7": "⁷", "8": "⁸", "9": "⁹",
  "+": "⁺", "-": "⁻", "=": "⁼", "(": "⁽", ")": "⁾",
  "a": "ᵃ", "b": "ᵇ", "c": "ᶜ", "d": "ᵈ", "e": "ᵉ", "f": "ᶠ", "g": "ᵍ",
  "h": "ʰ", "i": "ⁱ", "j": "ʲ", "k": "ᵏ", "l": "ˡ", "m": "ᵐ", "n": "ⁿ",
  "o": "ᵒ", "p": "ᵖ", "r": "ʳ", "s": "ˢ", "t": "ᵗ", "u": "ᵘ", "v": "ᵛ",
  "w": "ʷ", "x": "ˣ", "y": "ʸ", "z": "ᶻ",
  "A": "ᴬ", "B": "ᴮ", "D": "ᴰ", "E": "ᴱ", "G": "ᴳ", "H": "ᴴ", "I": "ᴵ",
  "J": "ᴶ", "K": "ᴷ", "L": "ᴸ", "M": "ᴹ", "N": "ᴺ", "O": "ᴼ", "P": "ᴾ",
  "R": "ᴿ", "T": "ᵀ", "U": "ᵁ", "V": "ⱽ", "W": "ᵂ",
  "α": "ᵅ", "β": "ᵝ", "γ": "ᵞ", "δ": "ᵟ", "ε": "ᵋ", "θ": "ᶿ",
};

const SUBSCRIPT_MAP: Record<string, string> = {
  "0": "₀", "1": "₁", "2": "₂", "3": "₃", "4": "₄",
  "5": "₅", "6": "₆", "7": "₇", "8": "₈", "9": "₉",
  "+": "₊", "-": "₋", "=": "₌", "(": "₍", ")": "₎",
  "a": "ₐ", "e": "ₑ", "h": "ₕ", "i": "ᵢ", "j": "ⱼ", "k": "ₖ", "l": "ₗ",
  "m": "ₘ", "n": "ₙ", "o": "ₒ", "p": "ₚ", "r": "ᵣ", "s": "ₛ", "t": "ₜ",
  "u": "ᵤ", "v": "ᵥ", "x": "ₓ",
};

/**
 * A script written with Unicode's raised or lowered characters when every
 * character has one; otherwise `^x` / `^(…)` (`_x` / `_(…)`), since a mix of
 * raised and level characters reads as a different formula.
 */
function toScript(str: string, map: Record<string, string>, mark: string): string {
  let res = "";
  for (const ch of str) {
    const mapped = map[ch];
    if (mapped === undefined) {
      return [...str].length === 1 ? mark + str : `${mark}(${str})`;
    }
    res += mapped;
  }
  return res;
}

function toSuperscript(str: string): string {
  return toScript(str, SUPERSCRIPT_MAP, "^");
}

function toSubscript(str: string): string {
  return toScript(str, SUBSCRIPT_MAP, "_");
}

const SCRIPT_CHARS = new Set([...Object.values(SUPERSCRIPT_MAP), ...Object.values(SUBSCRIPT_MAP), "′"]);

/** Index of the parenthesis closing the one at `open`, or -1. */
function matchingParen(str: string, open: number): number {
  let depth = 0;
  for (let i = open; i < str.length; i++) {
    if (str[i] === "(") depth++;
    else if (str[i] === ")" && --depth === 0) return i;
  }
  return -1;
}

/**
 * Whether a rendered fraction part reads as one term, so `a/b` needs no
 * parentheses around it: a number, a run of letters, a single symbol, or one
 * parenthesised group or root — each optionally followed by scripts, primes
 * or a factorial. `σ√(2π)` or `2σ²` is a product and gets parentheses.
 */
function isSingleTerm(str: string): boolean {
  const chars = [...str];
  while (chars.length > 1 && (SCRIPT_CHARS.has(chars[chars.length - 1]) || chars[chars.length - 1] === "!")) {
    chars.pop();
  }
  const core = chars.join("");
  if (chars.length <= 1) return true;
  if (/^[0-9]+(\.[0-9]+)?$/.test(core) || /^[\p{L}∂∇ℏℓ]+$/u.test(core)) return true;
  const open = core.startsWith("√(") ? 1 : core.startsWith("(") ? 0 : -1;
  return open >= 0 && matchingParen(core, open) === core.length - 1;
}

function extractBalancedBraced(str: string, startIndex: number): { content: string; start: number; end: number } | null {
  const braceIdx = str.indexOf("{", startIndex);
  if (braceIdx === -1) return null;
  if (braceIdx > 0 && str[braceIdx - 1] === "\\") return null;
  let depth = 0;
  for (let i = braceIdx; i < str.length; i++) {
    if (str[i] === "{" && (i === 0 || str[i - 1] !== "\\")) {
      depth++;
    } else if (str[i] === "}" && (i === 0 || str[i - 1] !== "\\")) {
      depth--;
      if (depth === 0) {
        return { content: str.slice(braceIdx + 1, i), start: braceIdx, end: i + 1 };
      }
    }
  }
  return null;
}

/** Render a LaTeX math formula to clear, readable Unicode mathematical notation */
export function renderLatexMath(latex: string): string {
  let s = latex.trim();

  // 1. Fractions (\frac{a}{b}) with balanced braces
  let changed = true;
  let passes = 0;
  while (changed && passes++ < 10) {
    changed = false;
    const idx = s.indexOf("\\frac");
    if (idx === -1) break;
    const num = extractBalancedBraced(s, idx + 5);
    if (!num) break;
    const den = extractBalancedBraced(s, num.end);
    if (!den) break;
    const rNum = renderLatexMath(num.content);
    const rDen = renderLatexMath(den.content);
    let repl: string;
    if (rNum === "1" && rDen === "2") repl = "½";
    else if (rNum === "1" && rDen === "4") repl = "¼";
    else if (rNum === "3" && rDen === "4") repl = "¾";
    else if (rNum === "1" && rDen === "3") repl = "⅓";
    else if (rNum === "2" && rDen === "3") repl = "⅔";
    else {
      const wrap = (part: string) => (isSingleTerm(part) ? part : `(${part})`);
      const wrapNum = wrap(rNum);
      const wrapDen = wrap(rDen);
      repl = `${wrapNum}/${wrapDen}`;
    }
    s = s.slice(0, idx) + repl + s.slice(den.end);
    changed = true;
  }

  // 1b. Binomials: \binom{n}{k} → C(n,k)
  passes = 0;
  while (passes++ < 10) {
    const idx = s.indexOf("\\binom");
    if (idx === -1) break;
    const top = extractBalancedBraced(s, idx + 6);
    if (!top) break;
    const bottom = extractBalancedBraced(s, top.end);
    if (!bottom) break;
    s = s.slice(0, idx) + `C(${renderLatexMath(top.content)},${renderLatexMath(bottom.content)})` + s.slice(bottom.end);
  }

  // 2. Square roots: \sqrt[n]{x} or \sqrt{x} with balanced braces
  changed = true;
  passes = 0;
  while (changed && passes++ < 10) {
    changed = false;
    const idx = s.indexOf("\\sqrt");
    if (idx === -1) break;
    let after = idx + 5;
    while (after < s.length && /\s/.test(s[after])) after++;
    let degree = "";
    if (s[after] === "[") {
      const closeB = s.indexOf("]", after);
      if (closeB !== -1) {
        degree = s.slice(after + 1, closeB).trim();
        after = closeB + 1;
      }
    }
    const inner = extractBalancedBraced(s, after);
    if (!inner) break;
    const rInner = renderLatexMath(inner.content);
    const degPrefix = degree ? toSuperscript(degree) : "";
    const repl = `${degPrefix}√(${rInner})`;
    s = s.slice(0, idx) + repl + s.slice(inner.end);
    changed = true;
  }

  // 3. Formatting macros: \mathbf{...}, \text{...}, etc.
  changed = true;
  passes = 0;
  const macroRe = /\\(mathbf|mathrm|mathit|mathsf|mathtt|text|textbf|textit|mathbb|mathcal|boldsymbol)\s*\{/;
  while (changed && passes++ < 10) {
    changed = false;
    const m = s.match(macroRe);
    if (!m || m.index === undefined) break;
    const inner = extractBalancedBraced(s, m.index + m[0].length - 1);
    if (!inner) break;
    let rInner = renderLatexMath(inner.content);
    if (m[1] === "mathbb" || m[1] === "mathcal") rInner = mathAlphabet(rInner, m[1]);
    s = s.slice(0, m.index) + rInner + s.slice(inner.end);
    changed = true;
  }
  // Accents: \hat{x} → x̂, \vec{v} → v⃗
  changed = true;
  passes = 0;
  const accentRe = new RegExp(`\\\\(${Object.keys(ACCENT_MAP).join("|")})\\s*\\{`);
  while (changed && passes++ < 10) {
    changed = false;
    const m = s.match(accentRe);
    if (!m || m.index === undefined) break;
    const inner = extractBalancedBraced(s, m.index + m[0].length - 1);
    if (!inner) break;
    const mark = ACCENT_MAP[m[1]];
    let rInner = "";
    for (const ch of renderLatexMath(inner.content)) rInner += ch === " " ? ch : ch + mark;
    s = s.slice(0, m.index) + rInner + s.slice(inner.end);
    changed = true;
  }
  s = s.replace(/\\operatorname\*?\s*\{([^{}]+)\}/g, "$1");

  // 4. Parens / spacing / layout. Environments (`aligned`, `cases`, …) are
  //    drawn a line at a time, so their delimiters, `&` alignment points and
  //    `\\` row breaks have nothing to draw.
  s = s.replace(/\\(begin|end)\s*\{[^{}]*\}(\{[^{}]*\})?/g, "");
  s = s.replace(/\\\\(\[[^\]]*\])?/g, " ");
  s = s.replace(/(?<!\\)&/g, " ");
  s = s.replace(/\\(displaystyle|textstyle|scriptstyle|limits|nolimits)\b/g, "");
  s = s.replace(/\\(left|right)\./g, "");
  s = s.replace(/\\(left|right|big|Big|bigg|Bigg)[lr]?\b/g, "");
  s = s.replace(/\\([,;:!])/g, " ");
  s = s.replace(/\\quad\b/g, "  ");
  s = s.replace(/\\qquad\b/g, "    ");
  s = s.replace(/\\\{/g, "{");
  s = s.replace(/\\\}/g, "}");
  s = s.replace(/\\([&%#$])/g, "$1");

  // 5. Functions
  s = s.replace(/\\(sin|cos|tan|arcsin|arccos|arctan|sinh|cosh|tanh|log|ln|exp|lim|min|max|deg|det|dim|gcd|ker|inf|sup)(?![A-Za-z])/g, "$1");

  // 6. Greek letters and symbols. A letter-like symbol joins the term it
  //    starts, as LaTeX ignores the space after a command: `\pi r` is πr,
  //    `\partial f` is ∂f.
  s = s.replace(/\\([A-Za-z]+)(?![A-Za-z])( (?=[\p{L}\p{N}]))?/gu, (match, word, space) => {
    if (GREEK_MAP[word] || LETTER_LIKE.has(word)) return GREEK_MAP[word] ?? SYMBOL_MAP[word];
    return (SYMBOL_MAP[word] ?? `\\${word}`) + (space ?? "");
  });

  // 7. Superscripts with balanced braces
  changed = true;
  passes = 0;
  while (changed && passes++ < 10) {
    changed = false;
    const idx = s.indexOf("^{");
    if (idx === -1) break;
    const inner = extractBalancedBraced(s, idx + 1);
    if (!inner) break;
    const rInner = renderLatexMath(inner.content);
    const repl = toSuperscript(rInner);
    s = s.slice(0, idx) + repl + s.slice(inner.end);
    changed = true;
  }
  // A bare `^(` is the fallback written above, not a script to raise again.
  s = s.replace(/\^([0-9a-zA-Z+\-=])/g, (_m, ch) => SUPERSCRIPT_MAP[ch] ?? ("^" + ch));

  // 8. Subscripts with balanced braces
  changed = true;
  passes = 0;
  while (changed && passes++ < 10) {
    changed = false;
    const idx = s.indexOf("_{");
    if (idx === -1) break;
    const inner = extractBalancedBraced(s, idx + 1);
    if (!inner) break;
    const rInner = renderLatexMath(inner.content);
    const repl = toSubscript(rInner);
    s = s.slice(0, idx) + repl + s.slice(inner.end);
    changed = true;
  }
  s = s.replace(/_([0-9a-zA-Z+\-=])/g, (_m, ch) => SUBSCRIPT_MAP[ch] ?? ("_" + ch));

  // 9. Clean up multiple spaces
  s = s.replace(/ {2,}/g, " ");

  return s.trim();
}
