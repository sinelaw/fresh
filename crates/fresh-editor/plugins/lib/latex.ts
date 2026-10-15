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

function toSuperscript(str: string): string {
  let res = "";
  for (const ch of str) res += SUPERSCRIPT_MAP[ch] ?? ch;
  return res;
}

function toSubscript(str: string): string {
  let res = "";
  for (const ch of str) res += SUBSCRIPT_MAP[ch] ?? ch;
  return res;
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
      const wrapNum = /[+\-=\s/]/.test(rNum) ? `(${rNum})` : rNum;
      const wrapDen = /[+\-=\s/]/.test(rDen) ? `(${rDen})` : rDen;
      repl = `${wrapNum}/${wrapDen}`;
    }
    s = s.slice(0, idx) + repl + s.slice(den.end);
    changed = true;
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
    const rInner = renderLatexMath(inner.content);
    s = s.slice(0, m.index) + rInner + s.slice(inner.end);
    changed = true;
  }
  s = s.replace(/\\operatorname\*?\s*\{([^{}]+)\}/g, "$1");

  // 4. Parens / spacing
  s = s.replace(/\\(left|right)\b/g, "");
  s = s.replace(/\\([,;:!])/g, " ");
  s = s.replace(/\\quad\b/g, "  ");
  s = s.replace(/\\qquad\b/g, "    ");
  s = s.replace(/\\\{/g, "{");
  s = s.replace(/\\\}/g, "}");

  // 5. Functions
  s = s.replace(/\\(sin|cos|tan|arcsin|arccos|arctan|sinh|cosh|tanh|log|ln|exp|lim|min|max|deg|det|dim|gcd|ker|inf|sup)\b/g, "$1");

  // 6. Greek letters and symbols
  s = s.replace(/\\([A-Za-z]+)(?![A-Za-z])/g, (match, word) => GREEK_MAP[word] || SYMBOL_MAP[word] || match);

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
  s = s.replace(/\^([0-9a-zA-Z+\-=()])/g, (_m, ch) => SUPERSCRIPT_MAP[ch] ?? ("^" + ch));

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
  s = s.replace(/_([0-9a-zA-Z+\-=()])/g, (_m, ch) => SUBSCRIPT_MAP[ch] ?? ("_" + ch));

  // 9. Clean up multiple spaces
  s = s.replace(/ {2,}/g, " ");

  return s.trim();
}
