#!/usr/bin/env python3
"""
Generate TypeScript type definitions from Rust op definitions.

This script parses src/ts_runtime.rs to extract op definitions and their
signatures, then generates types/fresh.d.ts with proper TypeScript types.
"""

import re
from pathlib import Path
from dataclasses import dataclass


@dataclass
class OpDefinition:
    """Represents a single op definition from Rust"""
    name: str
    rust_name: str
    params: list[tuple[str, str]]  # [(name, type), ...]
    return_type: str
    doc: str = ""


def parse_rust_type_to_ts(rust_type: str) -> str:
    """Convert Rust type to TypeScript type"""
    type_map = {
        "u32": "number",
        "u8": "number",
        "usize": "number",
        "i32": "number",
        "bool": "boolean",
        "String": "string",
        "()": "void",
    }
    return type_map.get(rust_type, rust_type)


def extract_ops_from_rust(rust_code: str) -> list[OpDefinition]:
    """Extract op definitions from Rust source code"""
    ops = []

    # Find all op2 functions
    # Pattern matches: #[op2...] ... fn op_fresh_xxx(...)
    op_pattern = re.compile(
        r'#\[op2(?:\((?:fast)?\))?\]\s*'
        r'(?:#\[string\]\s*)?'  # Optional string return type marker
        r'fn\s+(op_fresh_\w+)\s*\(\s*'
        r'([^)]*)'  # Parameters
        r'\)\s*(?:->\s*([^\s{]+))?',  # Return type
        re.MULTILINE | re.DOTALL
    )

    for match in op_pattern.finditer(rust_code):
        rust_name = match.group(1)
        params_str = match.group(2)
        return_type = match.group(3) or "()"

        # Parse parameters
        params = []
        if params_str.strip():
            # Split by comma, but be careful with nested types
            param_parts = []
            depth = 0
            current = ""
            for char in params_str:
                if char in "<([":
                    depth += 1
                elif char in ">)]":
                    depth -= 1
                elif char == "," and depth == 0:
                    param_parts.append(current.strip())
                    current = ""
                    continue
                current += char
            if current.strip():
                param_parts.append(current.strip())

            for param in param_parts:
                param = param.strip()
                # Skip state parameter
                if "OpState" in param or param.startswith("state:"):
                    continue

                # Parse #[string] attribute
                is_string = "#[string]" in param
                param = param.replace("#[string]", "").strip()

                # Extract name and type
                if ":" in param:
                    parts = param.split(":", 1)
                    name = parts[0].strip()
                    rust_t = parts[1].strip()

                    if is_string:
                        ts_type = "string"
                    else:
                        ts_type = parse_rust_type_to_ts(rust_t)

                    params.append((name, ts_type))

        # Convert return type
        ts_return = parse_rust_type_to_ts(return_type.strip())

        # Convert op name to JS method name
        # op_fresh_get_active_buffer_id -> getActiveBufferId
        js_name = rust_name.replace("op_fresh_", "")
        parts = js_name.split("_")
        js_name = parts[0] + "".join(word.capitalize() for word in parts[1:])

        ops.append(OpDefinition(
            name=js_name,
            rust_name=rust_name,
            params=params,
            return_type=ts_return,
        ))

    return ops


def generate_typescript_defs(ops: list[OpDefinition]) -> str:
    """Generate TypeScript definition file content"""

    # Group ops by category based on naming
    status_ops = []
    query_ops = []
    mutation_ops = []
    overlay_ops = []

    for op in ops:
        if op.name in ["setStatus", "debug"]:
            status_ops.append(op)
        elif op.name.startswith("get") or op.name.startswith("is"):
            query_ops.append(op)
        elif "Overlay" in op.name or "overlay" in op.name:
            overlay_ops.append(op)
        else:
            mutation_ops.append(op)

    def format_method(op: OpDefinition) -> str:
        params_str = ", ".join(f"{name}: {t}" for name, t in op.params)
        return f"  {op.name}({params_str}): {op.return_type};"

    output = f'''/**
 * Fresh Editor TypeScript Plugin API
 *
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generated from src/ts_runtime.rs by scripts/generate_ts_types.py
 *
 * This file provides type definitions for the Fresh editor's TypeScript plugin system.
 * Plugins have access to the global `editor` object which provides methods to:
 * - Query editor state (buffers, cursors, viewports)
 * - Modify buffer content (insert, delete text)
 * - Add visual decorations (overlays, highlighting)
 * - Interact with the editor UI (status messages, prompts)
 */

declare global {{
  /**
   * Global editor API object available to all TypeScript plugins
   */
  const editor: EditorAPI;
}}

/**
 * Buffer identifier (unique numeric ID)
 */
type BufferId = number;

/**
 * Main editor API interface
 */
interface EditorAPI {{
  // === Status and Logging ===
'''

    for op in status_ops:
        output += format_method(op) + "\n"

    output += "\n  // === Buffer Queries ===\n"
    for op in query_ops:
        output += format_method(op) + "\n"

    output += "\n  // === Buffer Mutations ===\n"
    for op in mutation_ops:
        output += format_method(op) + "\n"

    output += "\n  // === Overlay Operations ===\n"
    for op in overlay_ops:
        output += format_method(op) + "\n"

    output += '''}}

// Export for module compatibility
export {{}};
'''

    return output


def main():
    # Read the Rust source
    project_root = Path(__file__).parent.parent
    rust_file = project_root / "src" / "ts_runtime.rs"

    if not rust_file.exists():
        print(f"Error: {rust_file} not found")
        return 1

    rust_code = rust_file.read_text()

    # Extract ops
    ops = extract_ops_from_rust(rust_code)
    print(f"Found {len(ops)} ops:")
    for op in ops:
        print(f"  - {op.name}({', '.join(f'{n}: {t}' for n, t in op.params)}) -> {op.return_type}")

    # Generate TypeScript definitions
    ts_content = generate_typescript_defs(ops)

    # Write output
    types_dir = project_root / "types"
    types_dir.mkdir(exist_ok=True)

    output_file = types_dir / "fresh.d.ts"
    output_file.write_text(ts_content)
    print(f"\nGenerated {output_file}")

    return 0


if __name__ == "__main__":
    exit(main())
