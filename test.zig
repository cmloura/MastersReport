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
        try stdout.print("{s} ", .{print_token(value)});
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

pub fn scanName(str: []const u8) []const u8 {
    var newstr: [10]u8 = undefined;
    var i: usize = 0;
    for (str) |c| {
        if (is_letter(c)) {
            newstr[i] = c;
            i += 1;
        } else {
            return &newstr;
        }
    }
    return &newstr;
}

pub fn scan(str: []u8) []const tokenT {
    var tokenList: [25]tokenT = undefined;
    var i: usize = 0;
    var tokeni: usize = 0;
    var whilestr = str;

    while (whilestr.len > 0) {
        const c = whilestr[0];
        if (is_letter(c)) {
            const scannedstr = scanName(whilestr);
            if (std.mem.eql(u8, scannedstr, "lam")) {
                tokenList[tokeni] = tokenT.LamT;
            } else {
                tokenList[tokeni] = tokenT.ArgT;
            }
            i = scannedstr.len;
        } else {
            tokenList[tokeni] = switch (c) {
                '(' => tokenT.LParenT,
                ')' => tokenT.RparenT,
                '.' => tokenT.PeriodT,
                else => continue,
            };

            // if (c == ) {
            //     tokenList[tokeni] = tokenT.LParenT;
            //     i = 1;
            // }
            // if (c == ')') {
            //     tokenList[tokeni] = tokenT.RparenT;
            //     i = 1;
            // }
            // if (c == '.') {
            //     tokenList[tokeni] = tokenT.PeriodT;
            //     i = 1;
            // }
            // if (c == ' ') {
            //     whilestr = whilestr[1..];
            //     continue;
            // }
        }
        tokeni += 1;
        if (whilestr.len == 1)
            break;
        whilestr = whilestr[i..];
    }

    return &tokenList;
}

pub fn print_token(token: tokenT) []const u8 {
    switch (token) {
        tokenT.LamT => return "lam",
        tokenT.ArgT => return "argT",
        tokenT.LParenT => return "(",
        tokenT.PeriodT => return ".",
        tokenT.RparenT => return ")",
    }
}
