const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

const MAX_BYTES: usize = 10 * 1024 * 1024; // 10MB

pub fn read_file(allocator: std.mem.Allocator, path: []const u8) ![:0]u8 {
    const content = try std.fs.cwd().readFileAllocOptions(allocator, path, MAX_BYTES, null, .of(u8), 0);
    return content; // The caller is responsible for freeing the content.
}

pub fn show_file_tokens(allocator: std.mem.Allocator, path: []const u8, writer: *std.io.Writer) !void {
    const read_buffer = try read_file(allocator, path);
    defer allocator.free(read_buffer);

    var L = Lexer.init(read_buffer);
    while (true) {
        const token = L.next();
        if (token.tag == .eof) {
            break;
        }
        try writer.print("{s} \"{s}\"\n", .{ @tagName(token.tag), token.str(read_buffer) });
    }
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    var args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (args.len < 2) {
        try stdout.print("Usage: minicc <file>\n", .{});
    } else {
        for (args[1..]) |filename| {
            try show_file_tokens(alloc, filename, stdout);
        }
    }

    try stdout.flush();
}
