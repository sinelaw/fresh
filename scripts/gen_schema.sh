#!/bin/bash
# Generate JSON schemas for Fresh configuration and themes
set -e
cd "$(dirname "$0")/.."

cargo run --no-default-features --features dev-bins,runtime --bin generate_schema config > crates/fresh-editor/plugins/config-schema.json
echo "Generated plugins/config-schema.json"

mkdir -p crates/fresh-editor/plugins/schemas
cargo run --no-default-features --features dev-bins,runtime --bin generate_schema theme > crates/fresh-editor/plugins/schemas/theme.schema.json
echo "Generated plugins/schemas/theme.schema.json"
