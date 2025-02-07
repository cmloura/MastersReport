const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    var buf: [100]u8 = undefined;

    try stdout.print("Enter a lambda expression: ", .{});

    const lambdaexp = (try stdin.readUntilDelimiterOrEof(&buf, '\n')).?;
    try stdout.print("Expression: {s}\n", .{lambdaexp});

    const tlist = try scan(lambdaexp, allocator);
    defer allocator.free(tlist);

    for (tlist) |token| {
        try stdout.print("{s} ", .{print_token(token)});
    }
}

// Token Scanner
const Token = struct { kind: tokenT, value: []const u8 };

const expE = union(enum) {
    VarE: []const u8,
    LambdaE: struct { arg: []const u8, body: *expE },
    ApplyE: struct { func: *expE, arg: *expE },
};

const tokenT = enum {
    LamT,
    LParenT,
    RparenT,
    PeriodT,
    IdT,
};

const errors = error{ InputEndsButExpectedAnExpression, InputEndsButExpectedToken, TokenSeenButExpected, UnboundVariableSeen };

pub fn is_letter(c: u8) bool {
    return c >= 'a' and c <= 'z';
}

pub fn scanName(str: []const u8) []const u8 {
    var i: usize = 0;
    while (i < str.len and is_letter(str[i])) {
        i += 1;
    }
    return str[0..i];
}

pub fn scan(str: []u8, allocator: std.mem.Allocator) ![]Token {
    var tokenList = std.ArrayList(Token).init(allocator);
    defer tokenList.deinit();

    var whilestr = str;

    while (whilestr.len > 0) {
        const c = whilestr[0];

        if (is_letter(c)) {
            const scannedstr = scanName(whilestr);
            if (std.mem.eql(u8, scannedstr, "lam")) {
                try tokenList.append(Token{ .kind = tokenT.LamT, .value = "lam" });
            } else {
                try tokenList.append(Token{ .kind = tokenT.IdT, .value = scannedstr });
            }
            whilestr = whilestr[scannedstr.len..];

            while (whilestr.len > 0 and whilestr[0] == ' ') {
                whilestr = whilestr[1..];
            }
        } else {
            switch (c) {
                '(' => try tokenList.append(Token{ .kind = tokenT.LParenT, .value = "(" }),
                ')' => try tokenList.append(Token{ .kind = tokenT.RparenT, .value = ")" }),
                '.' => try tokenList.append(Token{ .kind = tokenT.PeriodT, .value = "." }),
                else => {
                    whilestr = whilestr[1..];
                    continue;
                },
            }
            whilestr = whilestr[1..];
        }
    }

    return try tokenList.toOwnedSlice();
}

pub fn print_token(token: Token) []const u8 {
    return token.value;
}

pub fn expect_token(expectedT: tokenT, tokens: []const Token) ![]const Token {
    if (tokens.len == 0) {
        return errors.InputEndsButExpectedToken;
    }

    const tok1 = tokens[0];
    if (tok1.kind == expectedT) {
        return tokens[1..];
    } else {
        return errors.TokenSeenButExpected;
    }
}

// Parser
pub fn parse_exp(allocator: std.mem.Allocator, tokens: []const Token) !struct { *expE, []const Token } {
    if (tokens.len == 0) {
        return errors.InputEndsButExpectedAnExpression;
    }

    const token = tokens[0];

    switch (token.kind) {
        .IdT => {
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .VarE = token.value };
            return .{ exp1, tokens[1..] };
        },
        .LParenT => {
            var tail = tokens[1..];

            if (tail.len > 0 and tail[0].kind == .LamT) {
                tail = tail[1..];
                if (tail.len == 0 or tail[0].kind != .IdT) {
                    return errors.TokenSeenButExpected;
                }

                const arg = tail[0].value;
                tail = tail[1..];

                tail = try expect_token(.PeriodT, tail);
                const result = try parse_exp(allocator, tail);
                const body = result[0];
                tail = result[1];

                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .LambdaE = .{ .arg = arg, .body = body } };
                tail = try expect_token(.RparenT, tail);
                return .{ exp1, tail };
            } else {
                const left = try parse_exp(allocator, tail);
                tail = left[1];
                const right = try parse_exp(allocator, tail);
                tail = right[1];
                const exp1 = try allocator.create(expE);

                exp1.* = expE{ .ApplyE = .{ .func = left[0], .arg = right[0] } };
                left = try expect_token(.RparenT, tail);
                return .{ exp1, tail };
            }
        },
        else => return errors.TokenSeenButExpected,
    }
}

pub fn convert_debruijn(allocator: std.mem.Allocator, expr: *expE, bound: std.ArrayList([]const u8)) !*expE {
    switch (expr.*) {
        .VarE => |term| {
            for (bound.items, 0..) |varname, i| {
                if (std.mem.eql(u8, varname, term)) {
                    const exp1 = try allocator.create(expE);
                    exp1.* = expE{ .VarE = (bound.items.len - i) };
                    return exp1;
                }
            }
            return errors.UnboundVariableSeen;
        },
        .LambdaE => |lambder| {
            try bound.append(lambder.arg);
            const body = try convert_debruijn(allocator, lambder.body, bound);
            bound.pop();
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = "", .body = body } };
            return exp1;
        },
        .ApplyE => |funcapp| {
            const func = try convert_debruijn(allocator, funcapp.func, bound);
            const arg = try convert_debruijn(allocator, funcapp.arg, bound);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .func = func, .arg = arg } };
            return exp1;
        },
    }
}
