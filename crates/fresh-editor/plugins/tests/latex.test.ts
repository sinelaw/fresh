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
  "√(a)/(√(b + √(c)))",
  "nested: a denominator with more than one term is parenthesised",
);
eq(renderLatexMath("\\unknown{y}"), "\\unknown{y}", "an unknown command is left as written");

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
