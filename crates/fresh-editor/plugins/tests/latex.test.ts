/** LaTeX math as Unicode text (`lib/latex.ts`). */
import { renderLatexMath } from "../lib/latex.ts";

let failures = 0;
function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

eq(renderLatexMath("x = 1"), "x = 1", "plain text passes through");
eq(renderLatexMath("\\alpha + \\beta = \\gamma"), "α + β = γ", "Greek letters by name");
eq(renderLatexMath("E = mc^2"), "E = mc²", "a one-character superscript");
eq(renderLatexMath("x_{i}^{2}"), "xᵢ²", "braced subscript and superscript");
eq(renderLatexMath("\\sum_{i=1}^{n} i"), "∑ᵢ₌₁ⁿ i", "a symbol with limits");
eq(renderLatexMath("\\frac{a}{b} + \\sqrt{x}"), "a/b + √(x)", "a fraction and a root");
eq(
  renderLatexMath("\\frac{\\sqrt{a}}{\\sqrt{b + \\sqrt{c}}}"),
  "√(a)/√(b + √(c))",
  "nested: a root is one term, so it needs no parentheses",
);
eq(renderLatexMath("\\frac{a+b}{c}"), "(a+b)/c", "a sum over something is parenthesised");
eq(
  renderLatexMath("\\frac{1}{\\sigma\\sqrt{2\\pi}}"),
  "1/(σ√(2π))",
  "a product in the denominator is parenthesised",
);
eq(renderLatexMath("\\frac{x^k}{k!}"), "xᵏ/k!", "scripts and a factorial stay one term");
eq(renderLatexMath("\\frac{\\partial f}{\\partial x}"), "∂f/∂x", "a partial derivative");
eq(renderLatexMath("e^{i\\pi}"), "e^(iπ)", "a script Unicode cannot raise is written with ^( )");
eq(renderLatexMath("\\sum_{k=0}^{\\infty} a_k"), "∑ₖ₌₀^∞ aₖ", "one character that cannot be raised");
eq(
  renderLatexMath("e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}"),
  "e^(-(x-μ)²/(2σ²))",
  "a fraction in an exponent",
);
eq(renderLatexMath("\\lim_{n \\to \\infty} x_n"), "lim_(n → ∞) xₙ", "a function name with a subscript");
eq(renderLatexMath("\\mathbb{R} \\subset \\mathbb{C}"), "ℝ ⊂ ℂ", "double-struck letters");
eq(renderLatexMath("\\mathcal{O}(n \\log n)"), "𝒪(n log n)", "script letters");
eq(renderLatexMath("\\hat{x} + \\vec{v}"), "x̂ + v⃗", "accents as combining marks");
eq(renderLatexMath("\\pi r^2"), "πr²", "no space after a Greek letter, as in LaTeX");
eq(renderLatexMath("\\nabla \\cdot E"), "∇ · E", "an operator after a letter-like symbol keeps its spacing");
eq(renderLatexMath("x &= a + b \\\\"), "x = a + b", "alignment points and row breaks of an environment line");
eq(renderLatexMath("\\begin{aligned}"), "", "an environment's delimiter draws nothing");
eq(renderLatexMath("\\left\\{ x \\mid x > 0 \\right\\}"), "{ x | x > 0 }", "sized delimiters");
eq(renderLatexMath("\\binom{n}{k}"), "C(n,k)", "a binomial coefficient");
eq(renderLatexMath("\\frac12 + y_{i}"), "½ + yᵢ", "\\frac takes single-character arguments without braces");
eq(renderLatexMath("\\sqrt2 + x^{3}"), "√(2) + x³", "so does \\sqrt, leaving later groups alone");
eq(renderLatexMath("\\frac\\alpha\\beta"), "α/β", "a command is an argument too");
eq(
  renderLatexMath(Array.from({ length: 12 }, (_, i) => `a_{${i}}`).join("+")),
  "a₀+a₁+a₂+a₃+a₄+a₅+a₆+a₇+a₈+a₉+a₁₀+a₁₁",
  "every one of many scripts is drawn",
);
eq(renderLatexMath("\\frac{a}"), "\\frac{a}", "a fraction missing its denominator is left as written");
eq(renderLatexMath("\\unknown{y}"), "\\unknown{y}", "an unknown command is left as written");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
