// Benchmarks for critical editor operations
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use editor::buffer::Buffer;
use editor::cursor::{Cursor, CursorId, Cursors};
use editor::event::Event;
use editor::state::EditorState;

/// Benchmark buffer insert operations
fn bench_buffer_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("buffer_insert");

    // Test with different text sizes
    for size in [10, 100, 1000, 10000] {
        group.throughput(Throughput::Bytes(size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let mut buffer = Buffer::new();
                let text = "a".repeat(size);
                buffer.insert(black_box(0), black_box(&text));
            });
        });
    }

    group.finish();
}

/// Benchmark buffer delete operations
fn bench_buffer_delete(c: &mut Criterion) {
    let mut group = c.benchmark_group("buffer_delete");

    // Test with different text sizes
    for size in [10, 100, 1000, 10000] {
        group.throughput(Throughput::Bytes(size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            let text = "a".repeat(size);
            let buffer = Buffer::from_str(&text);

            b.iter(|| {
                let mut buffer = buffer.clone();
                buffer.delete(black_box(0..size));
            });
        });
    }

    group.finish();
}

/// Benchmark line-to-byte conversion (line cache)
fn bench_line_to_byte(c: &mut Criterion) {
    let mut group = c.benchmark_group("line_to_byte");

    // Create a buffer with many lines
    let text = "line\n".repeat(1000);
    let buffer = Buffer::from_str(&text);

    group.bench_function("line_500", |b| {
        b.iter(|| {
            buffer.line_to_byte(black_box(500));
        });
    });

    group.bench_function("line_900", |b| {
        b.iter(|| {
            buffer.line_to_byte(black_box(900));
        });
    });

    group.finish();
}

/// Benchmark byte-to-line conversion (line cache)
fn bench_byte_to_line(c: &mut Criterion) {
    let mut group = c.benchmark_group("byte_to_line");

    // Create a buffer with many lines
    let text = "line\n".repeat(1000);
    let buffer = Buffer::from_str(&text);

    group.bench_function("byte_2500", |b| {
        b.iter(|| {
            buffer.byte_to_line(black_box(2500));
        });
    });

    group.bench_function("byte_4500", |b| {
        b.iter(|| {
            buffer.byte_to_line(black_box(4500));
        });
    });

    group.finish();
}

/// Benchmark cursor adjustment after insert
fn bench_cursor_adjustment_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("cursor_adjustment_insert");

    // Test with different numbers of cursors
    for cursor_count in [1, 10, 50, 100] {
        group.bench_with_input(
            BenchmarkId::from_parameter(cursor_count),
            &cursor_count,
            |b, &cursor_count| {
                // Create cursors at various positions
                let mut cursors = Cursors::new();
                for i in 0..cursor_count {
                    cursors.add(Cursor::new(i * 10));
                }

                b.iter(|| {
                    let mut cursors = cursors.clone();
                    cursors.adjust_after_insert(black_box(50), black_box(5));
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cursor adjustment after delete
fn bench_cursor_adjustment_delete(c: &mut Criterion) {
    let mut group = c.benchmark_group("cursor_adjustment_delete");

    // Test with different numbers of cursors
    for cursor_count in [1, 10, 50, 100] {
        group.bench_with_input(
            BenchmarkId::from_parameter(cursor_count),
            &cursor_count,
            |b, &cursor_count| {
                // Create cursors at various positions
                let mut cursors = Cursors::new();
                for i in 0..cursor_count {
                    cursors.add(Cursor::new(i * 10));
                }

                b.iter(|| {
                    let mut cursors = cursors.clone();
                    cursors.adjust_after_delete(black_box(50..55));
                });
            },
        );
    }

    group.finish();
}

/// Benchmark full event application (insert)
fn bench_event_application_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("event_application_insert");

    let mut state = EditorState::new(Buffer::new(), 80, 24);

    group.bench_function("insert_event", |b| {
        b.iter(|| {
            let mut state = state.clone();
            let event = Event::Insert {
                position: 0,
                text: "hello world".to_string(),
                cursor_id: CursorId(0),
            };
            state.apply(black_box(&event));
        });
    });

    group.finish();
}

/// Benchmark full event application (delete)
fn bench_event_application_delete(c: &mut Criterion) {
    let mut group = c.benchmark_group("event_application_delete");

    let buffer = Buffer::from_str("hello world");
    let mut state = EditorState::new(buffer, 80, 24);

    group.bench_function("delete_event", |b| {
        b.iter(|| {
            let mut state = state.clone();
            let event = Event::Delete {
                range: 0..5,
                deleted_text: "hello".to_string(),
                cursor_id: CursorId(0),
            };
            state.apply(black_box(&event));
        });
    });

    group.finish();
}

/// Benchmark complete editing workflow
fn bench_editing_workflow(c: &mut Criterion) {
    let mut group = c.benchmark_group("editing_workflow");

    group.bench_function("type_delete_sequence", |b| {
        b.iter(|| {
            let mut state = EditorState::new(Buffer::new(), 80, 24);

            // Type "hello world"
            for ch in "hello world".chars() {
                let cursor = state.cursors.primary();
                let event = Event::Insert {
                    position: cursor.position,
                    text: ch.to_string(),
                    cursor_id: state.cursors.primary_id(),
                };
                state.apply(&event);
            }

            // Delete " world"
            let event = Event::Delete {
                range: 5..11,
                deleted_text: " world".to_string(),
                cursor_id: state.cursors.primary_id(),
            };
            state.apply(&event);

            // Move cursor to start
            let event = Event::MoveCursor {
                cursor_id: state.cursors.primary_id(),
                position: 0,
                anchor: None,
            };
            state.apply(&event);
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_buffer_insert,
    bench_buffer_delete,
    bench_line_to_byte,
    bench_byte_to_line,
    bench_cursor_adjustment_insert,
    bench_cursor_adjustment_delete,
    bench_event_application_insert,
    bench_event_application_delete,
    bench_editing_workflow,
);
criterion_main!(benches);
