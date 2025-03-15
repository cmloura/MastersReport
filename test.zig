const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    var buf: [250]u8 = undefined;

    try stdout.print("Enter a lambda expression: ", .{});

    const lambdaexp = (try stdin.readUntilDelimiterOrEof(&buf, '\n')).?;
    try stdout.print("Expression: {s}\n", .{lambdaexp});

    const tlist = try scan(lambdaexp, allocator);
    defer allocator.free(tlist);

    for (tlist) |token| {
        try stdout.print("{s} ", .{print_token(token)});
    }
    try stdout.print("\n\n", .{});

    var dept_dict = std.StringHashMap(usize).init(allocator);
    defer dept_dict.deinit();

    const expstruct = try parse_exp(allocator, tlist);
    try print_exp(expstruct.resexp);

    const converted_exp = try convert_debruijn(allocator, expstruct.resexp, &dept_dict);
    try stdout.print("\n\nConverted Expression: ", .{});
    try print_debruijn_exp(converted_exp);
    try stdout.print("\n\n", .{});

    // const reduced_debruijn = try beta_reduce(allocator, 0, converted_exp);
    // std.debug.print("\n\nReduced De Bruijn: \n\n", .{});
    // try print_exp(reduced_debruijn);

    var stack = Stack(StackType).init(allocator);
    defer stack.deinit();

    const env = try Environment.init(allocator, null, null);
    var state = State{ .code = converted_exp, .env = env, .stack = &stack };

    while (try evalStep(allocator, &state)) {}
    try stdout.print("\n\nFinal Result: ", .{});
    try print_debruijn_exp(state.code);
    try stdout.print("\n", .{});

    try free_exp(allocator, expstruct.resexp);
    try free_exp(allocator, converted_exp);
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
            try printer.print("lam{s} . ", .{lambder.arg});
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

const sigma = union(enum) { sigma1: struct { index: usize, M: *expE, N: expE }, sigma2: struct { index: usize, M1: *expE, M2: *expE, N: *expE } };

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

pub fn convert_debruijn(allocator: std.mem.Allocator, expr: *expE, dept_dict: *std.StringHashMap(usize)) !*expE {
    switch (expr.*) {
        .VarE => |item| {
            if (dept_dict.get(item)) |temp_new_value| {
                const new_value = try std.fmt.allocPrint(allocator, "{d}", .{temp_new_value});
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .VarE = new_value };
                return exp1;
            } else {
                return error.UnboundVariableSeen;
            }
        },
        .LambdaE => |lambder| {
            var temp = std.StringHashMap(usize).init(allocator);
            var it2 = dept_dict.iterator();
            while (it2.next()) |entry| {
                if (dept_dict.get(entry.key_ptr.*)) |value| {
                    try temp.put(entry.key_ptr.*, value + 1);
                } else {
                    try temp.put(entry.key_ptr.*, 1);
                }
            }

            try temp.put(lambder.arg, 1);

            dept_dict.* = temp;
            const new_exp = try convert_debruijn(allocator, lambder.body, &temp);

            const exp2 = try allocator.create(expE);
            exp2.* = expE{ .LambdaE = .{ .arg = "", .body = new_exp } };

            return exp2;
        },
        .ApplyE => |app| {
            var left_dict = try dept_dict.clone();
            defer left_dict.deinit();
            const left = try convert_debruijn(allocator, app.func, &left_dict);
            const right = try convert_debruijn(allocator, app.arg, dept_dict);

            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .arg = right, .func = left } };
            return exp1;
        },
    }
}

// Reduction
const location = u8;
const identifier = []u8;

pub fn beta_reduce(allocator: std.mem.Allocator, depth: usize, expr: *expE) !*expE {
    switch (expr.*) {
        .VarE => return expr,
        .LambdaE => |lambder| {
            const newbod = try beta_reduce(allocator, depth + 1, lambder.body);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
            return exp1;
        },
        .ApplyE => |app| {
            const newfunc = try beta_reduce(allocator, depth, app.func);
            const newarg = try beta_reduce(allocator, depth, app.arg);

            if (newfunc.* == .LambdaE) {
                return try reduce(allocator, newfunc.LambdaE.body, newarg, 1);
            } else {
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .ApplyE = .{ .func = newfunc, .arg = newarg } };
                return exp1;
            }
        },
    }
}

pub fn reduce(allocator: std.mem.Allocator, M: *expE, N: *expE, index: usize) !*expE {
    switch (M.*) {
        .VarE => |vare| {
            const var_index = try std.fmt.parseInt(usize, vare, 10);
            if (var_index == index) {
                return N;
            } else {
                return M;
            }
        },
        .LambdaE => |lambder| {
            const newbod = try reduce(allocator, lambder.body, N, index + 1);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
            return exp1;
        },
        .ApplyE => |app| {
            const newfunc = try reduce(allocator, app.func, N, index);
            const newarg = try reduce(allocator, app.arg, N, index);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .func = newfunc, .arg = newarg } };
            return exp1;
        },
    }
}

// Krivine Machine
const Closure = struct {
    exp: *expE,
    env: *Environment,
};

const Environment = struct {
    head: ?*Closure,
    next: ?*Environment,

    pub fn init(allocator: std.mem.Allocator, head: ?*Closure, next: ?*Environment) !*Environment {
        const env = try allocator.create(Environment);
        env.* = .{ .head = head, .next = next };
        return env;
    }

    pub fn lookup(self: *Environment, index: usize) ?*Closure {
        var cur = self;
        var i = index;

        while (true) {
            if (i == 0) {
                return cur.head;
            }

            if (cur.next) |next| {
                cur = next;
                i = i - 1;
            } else {
                return null;
            }
        }
    }
};

const StackType = struct { c: *expE, oldenv: *Environment };

const State = struct { code: *expE, env: *Environment, stack: *Stack(StackType) };

pub fn Stack(comptime T: type) type {
    return struct {
        stack: std.ArrayList(T),
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{ .stack = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            self.stack.deinit();
        }

        pub fn push(self: *Self, val: T) !void {
            try self.stack.append(val);
        }

        pub fn pop(self: *Self) ?T {
            return self.stack.popOrNull();
        }

        pub fn peek(self: *Self) ?T {
            if (self.stack.items.len == 0) {
                return null;
            }
            return self.stack.items[self.stack.items.len - 1];
        }

        pub fn count(self: *Self) usize {
            return self.stack.items.len;
        }

        pub fn isEmpty(self: *Self) bool {
            return self.stack.items.len == 0;
        }
    };
}

pub fn evalStep(allocator: std.mem.Allocator, state: *State) !bool {
    switch (state.code.*) {
        .VarE => {
            std.debug.print("In the VarE portion of Krivine Machine\n", .{});
            if (state.env.lookup(0)) |closure| {
                state.code = closure.exp;
                state.env = closure.env;
            } else {
                return errors.UnboundVariableSeen;
            }
        },
        .LambdaE => |lambder| {
            if (state.stack.pop()) |top| {
                std.debug.print("Popping {any} from the stack!\n", .{top});
                const closure = try allocator.create(Closure);
                closure.* = .{ .exp = top.c, .env = top.oldenv };
                const env1 = try Environment.init(allocator, closure, state.env);
                state.code = lambder.body;
                state.env = env1;
            } else {
                const reduced_expr = try beta_reduce(allocator, 0, lambder.body);
                const new_lambda_exp = try allocator.create(expE);
                new_lambda_exp.* = .{ .LambdaE = .{ .arg = "", .body = reduced_expr } };
                state.code = new_lambda_exp;
                std.debug.print("Nothing on the stack. Returning False\n", .{});
                return false;
            }
        },
        .ApplyE => |app| {
            std.debug.print("In the ApplyE portion of Krivine Machine\n", .{});
            try state.stack.push(.{ .c = app.arg, .oldenv = state.env });
            state.code = app.func;
        },
    }
    return true;
}
