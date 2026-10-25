#!/bin/bash
# Run all codegen: JSON schemas, then fresh.d.ts and the plugin API reference
# (the d.ts reads config-schema.json, so schemas must come first)
set -e
cd "$(dirname "$0")/.."

cargo run --no-default-features --features dev-bins,runtime --bin generate_schema config > crates/fresh-editor/plugins/config-schema.json
echo "Generated plugins/config-schema.json"

mkdir -p crates/fresh-editor/plugins/schemas
cargo run --no-default-features --features dev-bins,runtime --bin generate_schema theme > crates/fresh-editor/plugins/schemas/theme.schema.json
echo "Generated plugins/schemas/theme.schema.json"

cargo run --no-default-features --features dev-bins,runtime --bin generate_schema package > crates/fresh-editor/plugins/schemas/package.schema.json
echo "Generated plugins/schemas/package.schema.json"

cargo test -p fresh-plugin-runtime --lib write_fresh_dts_file -- --ignored --quiet
echo "Generated plugins/lib/fresh.d.ts and docs/plugins/api/"
