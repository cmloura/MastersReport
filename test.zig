const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var buf: [100]u8 = undefined;

    try stdout.print("Enter a lambda expression: ", .{});

    const lambdaexp = (try stdin.readUntilDelimiterOrEof(&buf, '\n')).?;
    try stdout.print("Expression: {s}", .{lambdaexp});

    const tlist = scan(lambdaexp);
    for (tlist) |value| {
        try stdout.print("{any}\n", .{value});
    }
}

// Token Scanner
const tokenT = enum {
    LamT,
    LParenT,
    RparenT,
    PeriodT,
    ArgT,
};

pub fn is_letter(c: u8) bool {
    return c >= 97 and c <= 122;
}

pub fn is_next(str: []const u8, c: u8) bool {
    if (str.len == 0) {
        return false;
    }
    if (str[0] == c) {
        return true;
    } else {
        return false;
    }
}

pub fn scanName(str: []u8) struct { x: []const u8, y: []const u8 } {
    var newstr: []u8 = "";
    var i: usize = 0;
    while (str.len > 0) {
        const c = str[0];
        var str1 = str[1..];
        if (is_letter(c)) {
            newstr[i] = c;
            i += 1;
            str = str1;
        } else {
            return .{ newstr, str1 };
        }
    }
}

pub fn scan(str: []u8) []const tokenT {
    var tokenList = [_]tokenT{};
    var i: usize = 0;

    while (str.len > 0) {
        const c = str[0];
        if (str.len == 0) {
            return &tokenList;
        } else {
            if (is_letter(c)) {
                const strings = scanName(str);
                if (std.mem.eql(u8, strings.x, "lam")) {
                    tokenList[i] = tokenT.LamT;
                } else {
                    tokenList[i] = tokenT.ArgT;
                }
                str = strings.y;
            } else {
                if (c == '(') {
                    tokenList[i] = tokenT.LParenT;
                }
                if (c == ')') {
                    tokenList[i] = tokenT.RparenT;
                }
                if (c == '.') {
                    tokenList[i] = tokenT.PeriodT;
                }
            }
            i += 1;
        }
    }

    return tokenList;
}
