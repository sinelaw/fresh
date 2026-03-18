// Zig syntax highlighting test
const std = @import("std");

fn greet(name: []const u8) void {
    std.debug.print("Hello, {s}!\n", .{name});
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    greet("World");
    const x: i32 = 42;
    _ = x;
}
