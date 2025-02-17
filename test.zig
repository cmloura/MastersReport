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
    try stdout.print("\n\n", .{});

    var global_env = std.StringHashMap(usize).init(allocator);
    defer global_env.deinit();

    const expstruct = try parse_exp(allocator, tlist);
    try print_exp(expstruct.resexp);

    try gather_bindings(expstruct.resexp, &global_env, 0);

    var res_str = std.ArrayList([]const u8).init(allocator);
    defer {
        for (res_str.items) |item| {
            allocator.free(item);
        }
        res_str.deinit();
    }

    const converted_exp = try convert_debruijn(allocator, expstruct.resexp, &res_str, &global_env);
    try stdout.print("\n\nConverted Expression: ", .{});
    try print_debruijn_exp(converted_exp);

    try free_exp(allocator, expstruct.resexp);
    try free_exp(allocator, converted_exp);
}

fn gather_bindings(expr: *expE, global_env: *std.StringHashMap(usize), depth: usize) !void {
    switch (expr.*) {
        .VarE => |vare| {
            if (!global_env.contains(vare)) {
                try global_env.put(vare, depth);
            }
        },
        .LambdaE => |lambder| {
            try global_env.put(lambder.arg, depth);

            try gather_bindings(lambder.body, global_env, depth + 1);
        },
        .ApplyE => |funcapp| {
            try gather_bindings(funcapp.func, global_env, depth);
            try gather_bindings(funcapp.arg, global_env, depth);
        },
    }
}

pub fn free_exp(allocator: std.mem.Allocator, expr: *expE) !void {
    switch (expr.*) {
        .VarE => |vare| {
            if (vare.len > 0 and vare[0] >= '0' and vare[0] <= '9') {
                allocator.free(vare);
            }
        },
        .LambdaE => |lambder| {
            try free_exp(allocator, lambder.body);
        },
        .ApplyE => |funcapp| {
            try free_exp(allocator, funcapp.func);
            try free_exp(allocator, funcapp.arg);
        },
    }
    allocator.destroy(expr);
}

pub fn print_exp(headexp: *expE) !void {
    const printer = std.io.getStdOut().writer();
    switch (headexp.*) {
        .VarE => |vare| {
            try printer.print("{s} ", .{vare});
        },
        .LambdaE => |lambder| {
            try printer.print("lam {s} . ", .{lambder.arg});
            try print_exp(lambder.body);
        },
        .ApplyE => |funcapp| {
            try print_exp(funcapp.func);
            try print_exp(funcapp.arg);
        },
    }
}

pub fn print_debruijn_exp(headexp: *expE) !void {
    const printer = std.io.getStdOut().writer();
    switch (headexp.*) {
        .VarE => |vare| {
            try printer.print("{s}", .{vare});
        },
        .LambdaE => |lambder| {
            try printer.print("lam. ", .{});
            try print_debruijn_exp(lambder.body);
        },
        .ApplyE => |funcapp| {
            try printer.print("(", .{});
            try print_debruijn_exp(funcapp.func);
            try printer.print(")(", .{});
            try print_debruijn_exp(funcapp.arg);
            try printer.print(")", .{});
        },
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

const ParseResult = struct { resexp: *expE, tokenl: []const Token };
const ParseError = error{ InputEndsButExpectedAnExpression, TokenSeenButExpected, AllocatorError, OutOfMemory, InputEndsButExpectedToken, UnboundVariableSeen };

// Parser
pub fn parse_exp(allocator: std.mem.Allocator, tokens: []const Token) ParseError!ParseResult {
    var tail = tokens;
    var exp1 = try parse_singular(allocator, &tail);

    while (tail.len > 0 and tail[0].kind != .RparenT) {
        const arg = try parse_singular(allocator, &tail);
        const newexp = try allocator.create(expE);
        newexp.* = expE{ .ApplyE = .{ .func = exp1, .arg = arg } };
        exp1 = newexp;
    }

    return ParseResult{ .resexp = exp1, .tokenl = tail };
}

pub fn parse_singular(allocator: std.mem.Allocator, tailptr: *[]const Token) ParseError!*expE {
    var tail = tailptr.*;
    if (tail.len == 0) {
        return errors.InputEndsButExpectedAnExpression;
    }

    const token = tail[0];
    tail = tail[1..];

    switch (token.kind) {
        .IdT => {
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .VarE = token.value };
            tailptr.* = tail;
            return exp1;
        },
        .LParenT => {
            const sub: ParseResult = try parse_exp(allocator, tail);
            tail = sub.tokenl;
            tail = try expect_token(.RparenT, tail);
            tailptr.* = tail;
            return sub.resexp;
        },
        .LamT => {
            if (tail.len == 0 or tail[0].kind != .IdT) {
                return errors.TokenSeenButExpected;
            }
            const arg = tail[0].value;
            tail = tail[1..];
            tail = try expect_token(.PeriodT, tail);
            const body = try parse_exp(allocator, tail);
            tail = body.tokenl;

            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = arg, .body = body.resexp } };
            tailptr.* = tail;
            return exp1;
        },
        else => {
            return ParseError.TokenSeenButExpected;
        },
    }
}

pub fn convert_debruijn(allocator: std.mem.Allocator, expr: *expE, bound: *std.ArrayList([]const u8), global_env: *std.StringHashMap(usize)) !*expE {
    switch (expr.*) {
        .VarE => |term| {
            for (bound.items, 0..) |varname, i| {
                if (std.mem.eql(u8, varname, term)) {
                    const exp1 = try allocator.create(expE);
                    const strconv = try std.fmt.allocPrint(allocator, "{}", .{(bound.items.len - i)});
                    exp1.* = expE{ .VarE = strconv };
                    return exp1;
                }
            }

            if (global_env.get(term)) |depth| {
                const exp1 = try allocator.create(expE);
                const index_value = depth + bound.items.len;
                const strconv = try std.fmt.allocPrint(allocator, "{}", .{index_value});
                exp1.* = expE{ .VarE = strconv };
                return exp1;
            }

            return errors.UnboundVariableSeen;
        },
        .LambdaE => |lambder| {
            try bound.append(lambder.arg);
            const body = try convert_debruijn(allocator, lambder.body, bound, global_env);
            _ = bound.pop();
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = "", .body = body } };
            return exp1;
        },
        .ApplyE => |funcapp| {
            const func = try convert_debruijn(allocator, funcapp.func, bound, global_env);
            const arg = try convert_debruijn(allocator, funcapp.arg, bound, global_env);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .func = func, .arg = arg } };
            return exp1;
        },
    }
}
