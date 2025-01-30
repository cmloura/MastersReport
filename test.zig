const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var buf: [10]u8 = undefined;

    try stdout.print("Enter a lambda expression: ", .{});
    //try stdout.flush();

    const lambdaexp = (try stdin.readUntilDelimiterOrEof(&buf, '\n')).?;
    //const trimmed = std.mem.trimRight(u8, lambdaexp, "\n");
    try stdout.print("Expression: {s}", .{lambdaexp});
}
