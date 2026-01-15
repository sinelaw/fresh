#!/bin/bash
# Stress test runner - runs a specific test multiple times in parallel
# Aborts immediately on first failure and shows the output
# Usage: ./scripts/stress_test.sh <test_name> [iterations] [parallelism]
# Example: ./scripts/stress_test.sh e2e::recovery::test_recovery_after_save_with_size_change 100 16

set -e

TEST_NAME="${1:?Usage: $0 <test_name> [iterations] [parallelism]}"
ITERATIONS="${2:-100}"
PARALLELISM="${3:-16}"

# Build the test binary first
echo "Building test binary..."
cargo build --test e2e_tests 2>&1 | tail -3

# Find the test binary
TEST_BIN=$(find target/debug/deps -name 'e2e_tests-*' -type f -executable ! -name '*.d' | head -1)
if [ -z "$TEST_BIN" ]; then
    echo "Error: Could not find test binary"
    exit 1
fi
echo "Using binary: $TEST_BIN"

# Create temp dir for logs and control files
LOG_DIR=$(mktemp -d)
FAIL_FLAG="$LOG_DIR/failed"
echo "Logs in: $LOG_DIR"
echo "Running $ITERATIONS iterations with $PARALLELISM parallel..."
echo "Will abort on first failure."
echo ""

# Run tests in parallel, abort on first failure
run_test() {
    local i=$1
    local log="$LOG_DIR/test_${i}.log"

    # Check if another test already failed
    if [ -f "$FAIL_FLAG" ]; then
        exit 0
    fi

    "$TEST_BIN" "$TEST_NAME" --nocapture > "$log" 2>&1
    EXIT_CODE=$?

    if [ $EXIT_CODE -ne 0 ]; then
        # Mark failure and dump output
        echo "$i" > "$FAIL_FLAG"
        echo ""
        echo "=== FAILURE on run $i ==="
        echo ""
        cat "$log"
        exit 1
    else
        echo -n "."
    fi
}

export -f run_test
export TEST_BIN TEST_NAME LOG_DIR FAIL_FLAG

# Use GNU parallel if available, otherwise xargs
if command -v parallel &> /dev/null; then
    seq "$ITERATIONS" | parallel -j "$PARALLELISM" --halt now,fail=1 run_test {}
    RESULT=$?
else
    # xargs doesn't support early abort well, so we use a different approach
    seq "$ITERATIONS" | xargs -P "$PARALLELISM" -I {} bash -c '
        log="'"$LOG_DIR"'/test_{}.log"

        # Check if another test already failed
        if [ -f "'"$FAIL_FLAG"'" ]; then
            exit 0
        fi

        "'"$TEST_BIN"'" "'"$TEST_NAME"'" --nocapture > "$log" 2>&1
        EXIT_CODE=$?

        if [ $EXIT_CODE -ne 0 ]; then
            echo "{}" > "'"$FAIL_FLAG"'"
            echo ""
            echo "=== FAILURE on run {} ==="
            echo ""
            cat "$log"
            exit 1
        else
            echo -n "."
        fi
    '
    RESULT=$?
fi

echo ""
echo ""

if [ -f "$FAIL_FLAG" ]; then
    FAILED_RUN=$(cat "$FAIL_FLAG")
    echo "FAILED on run $FAILED_RUN"
    rm -rf "$LOG_DIR"
    exit 1
else
    # Count results
    TOTAL=$(ls "$LOG_DIR"/test_*.log 2>/dev/null | wc -l)
    PASSED=$(grep -l "test result: ok" "$LOG_DIR"/test_*.log 2>/dev/null | wc -l)
    echo "All $PASSED/$TOTAL tests passed"
    rm -rf "$LOG_DIR"
    exit 0
fi
