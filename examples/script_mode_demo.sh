#!/bin/bash
# Demo script showing how to use Fresh editor in script mode
# This demonstrates programmatic control of the editor via JSON commands

set -e

echo "=== Fresh Editor Script Mode Demo ==="
echo

# Check if fresh is built
if ! cargo build --quiet 2>/dev/null; then
    echo "Error: Failed to build fresh editor"
    exit 1
fi

echo "1. Starting editor in script mode and typing some code..."
echo

# Send a series of commands to the editor
cat << 'EOF' | cargo run --quiet -- --script-mode 2>/dev/null | while read -r line; do
    # Parse the response type
    type=$(echo "$line" | python3 -c "import json, sys; print(json.loads(sys.stdin.read())['type'])")

    case "$type" in
        "ok")
            msg=$(echo "$line" | python3 -c "import json, sys; d=json.loads(sys.stdin.read()); print(d.get('message', 'Success'))")
            echo "✓ $msg"
            ;;
        "screen")
            echo "--- Screen Render ---"
            echo "$line" | python3 -c "import json, sys; print(json.loads(sys.stdin.read())['content'])"
            echo "--------------------"
            ;;
        "status")
            echo "--- Status ---"
            echo "$line" | python3 -m json.tool
            echo "--------------"
            ;;
        "buffer")
            echo "--- Buffer Content ---"
            echo "$line" | python3 -c "import json, sys; print(json.loads(sys.stdin.read())['content'])"
            echo "---------------------"
            ;;
        "test_code")
            echo "--- Generated Test ---"
            echo "$line" | python3 -c "import json, sys; print(json.loads(sys.stdin.read())['code'])"
            echo "---------------------"
            ;;
        "error")
            msg=$(echo "$line" | python3 -c "import json, sys; print(json.loads(sys.stdin.read())['message'])")
            echo "✗ Error: $msg"
            ;;
    esac
done << 'COMMANDS'
{"type": "type_text", "text": "// Demo: Script Mode in Action"}
{"type": "key", "code": "Enter"}
{"type": "key", "code": "Enter"}
{"type": "type_text", "text": "fn greet(name: &str) {"}
{"type": "key", "code": "Enter"}
{"type": "type_text", "text": "    println!(\"Hello, {}!\", name);"}
{"type": "key", "code": "Enter"}
{"type": "type_text", "text": "}"}
{"type": "render"}
{"type": "status"}
{"type": "get_buffer"}
{"type": "export_test", "test_name": "test_demo_session"}
{"type": "quit"}
COMMANDS
EOF

echo
echo "2. Demo complete!"
echo
echo "Key features demonstrated:"
echo "  - Typing text programmatically"
echo "  - Sending keyboard events (Enter, etc.)"
echo "  - Rendering the screen"
echo "  - Getting editor status"
echo "  - Retrieving buffer content"
echo "  - Generating test code from interactions"
echo
echo "This script mode enables:"
echo "  - LLM control of the editor"
echo "  - Automated testing"
echo "  - Integration with external tools"
echo "  - Recording and replaying editor sessions"
