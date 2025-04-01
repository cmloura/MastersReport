const std = @import("std");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    var freed_nodes = std.AutoHashMap(*expE, void).init(allocator);
    var used_vars = std.StringHashMap([]const u8).init(allocator);
    defer used_vars.deinit();

    var buf1: [500]u8 = undefined;
    var buf2: [500]u8 = undefined;

    try stdout.print("Enter a lambda expression: ", .{});

    const lambdaexp1 = (try stdin.readUntilDelimiterOrEof(&buf1, '\n')).?;

    try stdout.print("Enter another lambda expression: ", .{});

    const lambdaexp2 = (try stdin.readUntilDelimiterOrEof(&buf2, '\n')).?;

    const tlist1 = try scan(lambdaexp1, allocator, &used_vars);
    defer allocator.free(tlist1);

    const tlist2 = try scan(lambdaexp2, allocator, &used_vars);
    defer allocator.free(tlist2);

    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const expstruct = try parse_exp(allocator, tlist1);
    std.debug.print("\nAfter parsing: ", .{});
    try print_exp(allocator, expstruct.resexp);
    const secondexpstruct = try parse_exp(allocator, tlist2);

    const converted_exp = try convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);
    try stdout.print("\n\nConverted Expression: ", .{});
    try print_debruijn_exp(allocator, converted_exp);
    try stdout.print("\n\nSecond Converted Expression: ", .{});
    try print_debruijn_exp(allocator, second_converted_exp);

    var stack = Stack(StackType).init(allocator);
    defer stack.deinit();

    //const env = try Environment.init(allocator, null, null);
    //var state = State{ .code = converted_exp, .env = env, .stack = &stack };

    try stdout.print("\n\nCorrect Result for exp1: ", .{});
    const final_exp = try correct_beta_reduce(allocator, converted_exp);
    try print_debruijn_exp(allocator, final_exp);
    const final_exp2 = try correct_beta_reduce(allocator, second_converted_exp);
    try stdout.print("\n\nCorrect Result for exp1: ", .{});
    try print_debruijn_exp(allocator, final_exp2);
    //try print_debruijn_exp(state.code);
    try stdout.print("\n", .{});
    //while (try evalStep(allocator, &state)) {}
    //try stdout.print("\n\nKrivine Machine + Beta Reduction result: ", .{});
    //try print_debruijn_exp(allocator, state.code);

    const pair = DisagreementPair{ .left = final_exp, .right = final_exp2 };
    var pairs = std.ArrayList(DisagreementPair).init(allocator);
    defer pairs.deinit();
    try pairs.append(pair);

    var variables = std.StringHashMap(void).init(allocator);
    defer variables.deinit();

    try stdout.print("\nUnification time\n", .{});
    const simplified_res = try simplify(allocator, pairs.items);

    switch (simplified_res.type) {
        .Success => {
            try stdout.print("Yippee! Unification Successful\n", .{});
            if (simplified_res.substitution) |subst| {
                try stdout.print("Substitutions: \n", .{});
                var it = subst.iterator();
                while (it.next()) |entry| {
                    try stdout.print("{s} -> ", .{entry.key_ptr.*});
                    try print_debruijn_exp(allocator, entry.value_ptr.*);
                    try stdout.print("\n\n", .{});
                }
            }
        },
        .Failure => {
            try stdout.print("Unification Failed\n", .{});
        },
        .Intermediate => {
            try stdout.print("Intermediate result. Further processing required", .{});
        },
    }

    try free_exp(allocator, expstruct.resexp, &freed_nodes);
    try free_exp(allocator, converted_exp, &freed_nodes);
    try free_exp(allocator, final_exp, &freed_nodes);
    try free_exp(allocator, second_converted_exp, &freed_nodes);
}

pub fn free_exp(allocator: std.mem.Allocator, expr: *expE, freed_nodes: *std.AutoHashMap(*expE, void)) !void {
    if (freed_nodes.contains(expr)) {
        return;
    } else {
        try freed_nodes.put(expr, {});
        switch (expr.*) {
            .VarE => |vare| {
                var is_debruijn = true;
                for (vare) |c| {
                    if (c < '0' or c > '9') {
                        is_debruijn = false;
                        break;
                    }
                }
                if (is_debruijn and vare.len > 0) {
                    allocator.free(vare);
                }
            },
            .LambdaE => |lambder| {
                try free_exp(allocator, lambder.body, freed_nodes);
            },
            .ApplyE => |funcapp| {
                try free_exp(allocator, funcapp.func, freed_nodes);
                try free_exp(allocator, funcapp.arg, freed_nodes);
            },
            .UnboundVarE => {
                //allocator.free(uvare);
            },
            .BoundVarE => {},
            .MetavarE => {},
        }
        allocator.destroy(expr);
    }
}

pub fn print_exp(allocator: std.mem.Allocator, headexp: *expE) !void {
    const printer = std.io.getStdOut().writer();
    switch (headexp.*) {
        .VarE => |vare| {
            try printer.print("{s} ", .{vare});
        },
        .LambdaE => |lambder| {
            try printer.print("lam{s} . ", .{lambder.arg});
            try print_exp(allocator, lambder.body);
        },
        .ApplyE => |funcapp| {
            try print_exp(allocator, funcapp.func);
            try print_exp(allocator, funcapp.arg);
        },
        .BoundVarE => |bvare| {
            const bvarestr = try std.fmt.allocPrint(allocator, "{d}", .{bvare});
            try printer.print("{s} ", .{bvarestr});
        },
        .UnboundVarE => |uvare| {
            try printer.print("{s} ", .{uvare});
        },
        .MetavarE => |metavar| {
            try printer.print("{s}", .{metavar});
        },
    }
}

pub fn print_debruijn_exp(allocator: std.mem.Allocator, headexp: *expE) !void {
    const printer = std.io.getStdOut().writer();
    switch (headexp.*) {
        .VarE => |vare| {
            try printer.print("{s}", .{vare});
        },
        .LambdaE => |lambder| {
            try printer.print("lam. ", .{});
            try print_debruijn_exp(allocator, lambder.body);
        },
        .ApplyE => |funcapp| {
            try printer.print("(", .{});
            try print_debruijn_exp(allocator, funcapp.func);
            try printer.print(")(", .{});
            try print_debruijn_exp(allocator, funcapp.arg);
            try printer.print(")", .{});
        },
        .BoundVarE => |bvare| {
            //std.debug.print("Bound Variable: ", .{});
            const bvarestr = try std.fmt.allocPrint(allocator, "{d}", .{bvare});
            try printer.print("{s}", .{bvarestr});
        },
        .UnboundVarE => |uvare| {
            //std.debug.print("Unbound Variable: ", .{});
            try printer.print("{s}", .{uvare});
        },
        .MetavarE => |metavar| {
            try printer.print("{s}", .{metavar});
        },
    }
}

// Token Scanner
const Token = struct { kind: tokenT, value: []const u8 };

const expE = union(enum) { VarE: []const u8, LambdaE: struct { arg: []const u8, body: *expE }, ApplyE: struct { func: *expE, arg: *expE }, UnboundVarE: []const u8, BoundVarE: usize, MetavarE: []const u8 };

const tokenT = enum { LamT, LParenT, RparenT, PeriodT, IdT, MetavariableT };

const errors = error{ InputEndsButExpectedAnExpression, InputEndsButExpectedToken, TokenSeenButExpected, UnboundVariableSeen, InvalidFlexibleTerm };

pub fn is_lowercase_letter(c: u8) bool {
    return c >= 'a' and c <= 'z';
}

pub fn is_uppercase_letter(c: u8) bool {
    return c >= 'A' and c <= 'Z';
}

pub fn scanName(str: []const u8) []const u8 {
    var i: usize = 0;
    while (i < str.len and is_lowercase_letter(str[i])) {
        i += 1;
    }
    return str[0..i];
}

pub fn scanMetavariable(str: []const u8) []const u8 {
    var i: usize = 0;
    while (i < str.len and is_uppercase_letter(str[i])) {
        i += 1;
    }
    return str[0..i];
}

pub fn scan(str: []u8, allocator: std.mem.Allocator, used_vars: *std.StringHashMap([]const u8)) ![]Token {
    var tokenList = std.ArrayList(Token).init(allocator);
    defer tokenList.deinit();

    var whilestr = str;

    while (whilestr.len > 0) {
        const c = whilestr[0];

        if (is_lowercase_letter(c)) {
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
        } else if (is_uppercase_letter(c)) {
            const scannedMetavar = scanMetavariable(whilestr);
            try tokenList.append(Token{ .kind = tokenT.MetavariableT, .value = scannedMetavar });
            try used_vars.put(scannedMetavar, {});

            whilestr = whilestr[scannedMetavar.len..];
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
        .MetavariableT => {
            const arg = token.value;
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .MetavarE = arg };
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
            const exp1 = try allocator.create(expE);
            if (dept_dict.get(item)) |temp_new_value| {
                exp1.* = expE{ .BoundVarE = temp_new_value };
                return exp1;
            } else {
                exp1.* = expE{ .UnboundVarE = item };
                return exp1;
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
        .MetavarE => {
            return expr;
        },
        else => {
            return ParseError.UnboundVariableSeen; // Any other expression types are created during the De Bruijn Process
        },
    }
}

pub fn shift_indices(allocator: std.mem.Allocator, d: usize, expr: *expE, cutoff: usize) !*expE {
    switch (expr.*) {
        .VarE => |index_str| {
            const index = std.fmt.parseInt(usize, index_str, 10) catch {
                return try copy_expr(allocator, expr);
            };
            if (index >= cutoff) {
                const new_index = try std.fmt.allocPrint(allocator, "{d}", .{index + d});
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .VarE = new_index };
                return exp1;
            }
            return expr;
        },
        .UnboundVarE => {
            return try copy_expr(allocator, expr);
        },
        .MetavarE => {
            return try copy_expr(allocator, expr);
        },
        .BoundVarE => |bvare| {
            if (bvare >= cutoff) {
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .BoundVarE = bvare + d };
                return exp1;
            }
            return expr;
        },
        .LambdaE => |lambder| {
            const newbod = try shift_indices(allocator, d, lambder.body, cutoff + 1);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
            return exp1;
        },
        .ApplyE => |app| {
            const newfunc = try shift_indices(allocator, d, app.func, cutoff);
            const newarg = try shift_indices(allocator, d, app.arg, cutoff);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .func = newfunc, .arg = newarg } };
            return exp1;
        },
    }
}

pub fn copy_expr(allocator: std.mem.Allocator, expr: *expE) !*expE {
    const exp1 = try allocator.create(expE);
    switch (expr.*) {
        .VarE => |index_str| {
            const new_index = try allocator.dupe(u8, index_str);
            exp1.* = expE{ .VarE = new_index };
        },
        .BoundVarE => |bvare| {
            exp1.* = expE{ .BoundVarE = bvare };
        },
        .UnboundVarE => |uvare| {
            const new_index = try allocator.dupe(u8, uvare);
            exp1.* = expE{ .UnboundVarE = new_index };
        },
        .LambdaE => |lambder| {
            const new_body = try copy_expr(allocator, lambder.body);
            const new_arg = try allocator.dupe(u8, lambder.arg);
            exp1.* = expE{ .LambdaE = .{ .arg = new_arg, .body = new_body } };
        },
        .ApplyE => |app| {
            const new_func = try copy_expr(allocator, app.func);
            const new_arg = try copy_expr(allocator, app.arg);
            exp1.* = expE{ .ApplyE = .{ .func = new_func, .arg = new_arg } };
        },
        .MetavarE => |metavar| {
            const new_index = try allocator.dupe(u8, metavar);
            exp1.* = expE{ .MetavarE = new_index };
        },
    }
    return exp1;
}

pub fn substitute(allocator: std.mem.Allocator, M: *expE, N: *expE, j: usize) !*expE {
    switch (M.*) {
        .VarE => |index_str| {
            const index = try std.fmt.parseInt(usize, index_str, 10);
            if (index == j + 1) {
                return try copy_expr(allocator, N);
            } else if (index > j + 1) {
                const new_index = try std.fmt.allocPrint(allocator, "{d}", .{index - 1});
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .VarE = new_index };
                return exp1;
            } else {
                return try copy_expr(allocator, M);
            }
        },
        .UnboundVarE => {
            return try copy_expr(allocator, M);
        },
        .MetavarE => {
            return try copy_expr(allocator, M);
        },
        .BoundVarE => |bvare| {
            if (bvare == j + 1) {
                return try copy_expr(allocator, N);
            } else if (bvare > j + 1) {
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .BoundVarE = bvare - 1 };
                return exp1;
            } else {
                return try copy_expr(allocator, M);
            }
        },
        .LambdaE => |lambder| {
            const shifted_N = try shift_indices(allocator, 1, N, 1);
            const newbod = try substitute(allocator, lambder.body, shifted_N, j + 1);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
            return exp1;
        },
        .ApplyE => |app| {
            const new_func = try substitute(allocator, app.func, N, j);
            const new_arg = try substitute(allocator, app.arg, N, j);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .ApplyE = .{ .func = new_func, .arg = new_arg } };
            return exp1;
        },
    }
}

pub fn correct_beta_reduce(allocator: std.mem.Allocator, expr: *expE) !*expE {
    switch (expr.*) {
        .ApplyE => |app| {
            const reduced_func = try correct_beta_reduce(allocator, app.func);
            const reduced_arg = try correct_beta_reduce(allocator, app.arg);

            if (reduced_func.* == .LambdaE) {
                const lambda = reduced_func.*.LambdaE;
                if (reduced_arg.* == .VarE or reduced_arg.* == .BoundVarE) {
                    return try substitute(allocator, lambda.body, reduced_arg, 0);
                } else {
                    const new_left = try substitute(allocator, lambda.body, reduced_arg, 0);
                    return try correct_beta_reduce(allocator, new_left);
                }
            } else {
                const exp1 = try allocator.create(expE);
                exp1.* = expE{ .ApplyE = .{ .func = reduced_func, .arg = reduced_arg } };
                return exp1;
            }
        },
        .LambdaE => |lambder| {
            const reduced_body = try correct_beta_reduce(allocator, lambder.body);
            const exp1 = try allocator.create(expE);
            exp1.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = reduced_body } };
            return exp1;
        },
        else => return expr,
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

// Higher order unification

const DisagreementPair = struct { left: *expE, right: *expE };

const Substitution = std.StringHashMap(*expE);

const Node = struct {
    type: NodeType,
    disagreement_pairs: []const DisagreementPair,
    substitution: ?Substitution = null,
};

const NodeType = enum {
    Failure,
    Success,
    Intermediate,
};

pub fn is_rigid(expr: *expE) bool {
    return switch (expr.*) {
        .BoundVarE => true,
        .VarE => false,
        .MetavarE => false,
        .UnboundVarE => false,
        .LambdaE => |lambder| is_rigid(lambder.body),
        .ApplyE => |app| is_rigid(app.func) and is_rigid(app.arg),
    };
}

pub fn same_head(left: *expE, right: *expE) bool {
    return switch (left.*) {
        .BoundVarE => |l_var| switch (right.*) {
            .BoundVarE => |r_var| l_var == r_var,
            else => false,
        },
        .VarE => |l_var| switch (right.*) {
            .VarE => |r_var| std.mem.eql(u8, l_var, r_var),
            else => false,
        },
        .ApplyE => |l_app| switch (right.*) {
            .ApplyE => |r_app| same_head(l_app.func, r_app.func),
            else => false,
        },
        .LambdaE => |l_lambder| switch (right.*) {
            .LambdaE => |r_lambder| same_head(l_lambder.body, r_lambder.body),
            else => false,
        },
        else => false,
    };
}

pub fn simplify(allocator: std.mem.Allocator, pairs: []const DisagreementPair) !Node {
    var simplified_pairs = std.ArrayList(DisagreementPair).init(allocator);
    defer simplified_pairs.deinit();

    for (pairs) |pair| {
        if (is_rigid(pair.left) and is_rigid(pair.right)) {
            if (!same_head(pair.left, pair.right)) {
                return Node{ .type = .Failure, .disagreement_pairs = &[_]DisagreementPair{} };
            }

            try breakdown(pair, &simplified_pairs);
            continue;
        }

        try simplified_pairs.append(pair);
    }

    var normal_pairs = std.ArrayList(DisagreementPair).init(allocator);
    defer normal_pairs.deinit();

    for (simplified_pairs.items) |pair| {
        if (is_rigid(pair.left) and !is_rigid(pair.right)) {
            try normal_pairs.append(.{ .left = pair.right, .right = pair.left });
        } else {
            try normal_pairs.append(pair);
        }
    }
    if (check_flexibility(normal_pairs.items)) {
        return create_success_node(allocator, normal_pairs.items);
    }

    const final = try allocator.dupe(DisagreementPair, normal_pairs.items);
    return Node{ .type = .Intermediate, .disagreement_pairs = final };
}

pub fn check_flexibility(pairs: []const DisagreementPair) bool {
    for (pairs) |pair| {
        if (is_rigid(pair.left) or is_rigid(pair.right)) {
            return false;
        }
    }
    return true;
}

pub fn breakdown(pair: DisagreementPair, res: *std.ArrayList(DisagreementPair)) !void {
    switch (pair.left.*) {
        .ApplyE => |l_app| switch (pair.right.*) {
            .ApplyE => |r_app| {
                try res.append(.{ .left = l_app.func, .right = r_app.func });
                try res.append(.{ .left = l_app.arg, .right = r_app.arg });
            },
            else => {},
        },
        .LambdaE => |l_lambder| switch (pair.right.*) {
            .LambdaE => |r_lambder| {
                try res.append(.{ .left = l_lambder.body, .right = r_lambder.body });
            },
            else => {},
        },
        else => {},
    }
}

pub fn create_success_node(allocator: std.mem.Allocator, pairs: []const DisagreementPair) !Node {
    var subst = Substitution.init(allocator);

    for (pairs) |pair| {
        switch (pair.left.*) {
            .MetavarE => |metavar| {
                try subst.put(metavar, pair.right);
            },
            .UnboundVarE => |uvare| {
                try subst.put(uvare, pair.right);
            },
            else => {},
        }
    }

    return Node{ .type = .Success, .disagreement_pairs = pairs, .substitution = subst };
}

pub fn generate_fresh_var(allocator: std.mem.Allocator, used_vars: *std.StringHashMap([]const u8)) []const u8 {
    var start: u8 = 'A';
    while (start <= 'Z') {
        if (!used_vars.contains(start)) {
            const res = try allocator.dupe(u8, start);
            try used_vars.put(res, {});
            return res;
        }
        start = start + 1;
    }

    var i: usize = 1;
    while (true) : (i += 1) {
        var c: u8 = 'A';
        while (c <= 'Z') : (c += 1) {
            const var_name = try std.fmt.allocPrint(allocator, "{c}{d}", .{ c, i });
            defer allocator.free(var_name);

            if (!used_vars.contains(var_name)) {
                const result = try allocator.dupe(u8, var_name);
                try used_vars.put(result, {});
                return result;
            }
        }
    }
}

pub fn get_head(exp: *expE) *expE {
    switch (exp.*) {
        .ApplyE => {
            var curr = exp;
            while (curr.* == .ApplyE) {
                curr = &curr.ApplyE.func.*;
            }
            return curr;
        },
        else => return exp,
    }
}

pub fn is_constant(exp: *expE) bool {
    return switch (exp.*) {
        .UnboundVarE => true,
        else => false,
    };
}

pub fn get_args(allocator: std.mem.Allocator, exp: *expE) !std.ArrayList(*expE) {
    var args = std.ArrayList(*expE).init(allocator);
    //defer args.deinit();

    var cur = exp;
    var args_list = std.ArrayList(*expE).init(allocator);
    defer args_list.deinit();

    while (cur.* == .ApplyE) {
        try args_list.append(&cur.ApplyE.arg.*);
        cur = &cur.ApplyE.func.*;
    }

    var i = args_list.items.len;
    while (i > 0) {
        i -= 1;
        const arg = try copy_expr(allocator, args_list.items[i]);
        try args.append(arg);
    }
    return args;
}

pub fn create_imitation_subst(allocator: std.mem.Allocator, metavar: []const u8, rigid_head: *expE, flex_args_len: usize, rigid_args_len: usize, variables: *std.StringHashMap(void)) !Substitution {
    var cur_exp = undefined;
    var lambder_bod = undefined;

    const head_clone = switch (rigid_head.*) {
        .UnboundVarE => |uvare| blk: {
            const name = try allocator.dupe(u8, uvare);
            const head = try allocator.create(expE);
            head.* = expE{ .UnboundVarE = name };
            break :blk head;
        },
        else => unreachable,
    };

    cur_exp = head_clone;
    for (0..rigid_args_len) |_| {
        const fresh_var = generate_fresh_var(allocator, variables);
        const metavar_exp = try allocator.create(expE);
        metavar_exp.* = expE{ .MetavarE = fresh_var };

        var param_app = metavar_exp;
        for (0..flex_args_len) |j| {
            const param_index = try allocator.create(expE);
            param_index.* = expE{ .BoundVarE = flex_args_len - j - 1 };

            const new_app = try allocator.create(expE);
            new_app.* = expE{ .ApplyE = .{ .func = cur_exp, .arg = param_app } };
            param_app = new_app;
        }
        const new_app2 = try allocator.create(expE);
        new_app2.* = expE{ .ApplyE = .{ .func = cur_exp, .arg = param_app } };
        cur_exp = new_app2;
    }

    lambder_bod = cur_exp;
    var replace = lambder_bod;
    for (0..flex_args_len) |i| {
        const param_name = try std.fmt.allocPrint(allocator, "z_{d}", .{flex_args_len - i - 1});
        const new_lambder = try allocator.create(expE);
        new_lambder.* = expE{ .LambdaE = .{ .arg = param_name, .body = replace } };
        replace = new_lambder;
    }
    const subst = Substitution.init(allocator);
    const var_name = try allocator.dupe(u8, metavar);
    try subst.put(var_name, replace);
    return subst;
}

pub fn create_projection_subst(allocator: std.mem.Allocator, metavar: []const u8, projection_index: usize, flex_args_len: usize, result_args_len: usize, variables: *std.StringHashMap(void)) !Substitution {
    var lambder_bod: *expE = undefined;

    const proj_var = try allocator.create(expE);
    proj_var.* = expE{ .BoundVarE = flex_args_len - projection_index - 1 };

    const cur_exp = proj_var;

    for (0..result_args_len) |_| {
        const fresh_var = try generate_fresh_var(allocator, variables);
        const metavar_exp = try allocator.create(expE);
        metavar_exp.* = expE{ .MetavarE = fresh_var };

        var param_app = metavar_exp;
        for (0..flex_args_len) |j| {
            const param_index = try allocator.create(expE);
            param_index.* = expE{ .BoundVarE = flex_args_len - j - 1 };

            const new_app = try allocator.create(expE);
            new_app.* = expE{ .ApplyE = .{ .func = param_app, .arg = param_index } };
            param_app = new_app;
        }

        const new_app = try allocator.create(expE);
        new_app.* = expE{ .ApplyE = .{ .func = cur_exp, .arg = param_app } };
    }

    lambder_bod = cur_exp;
    var replace = lambder_bod;
    for (0..flex_args_len) |i| {
        const param_name = try std.fmt.allocPrint(allocator, "z_{d}", .{flex_args_len - i - 1});
        const new_lambder = try allocator.create(expE);
        new_lambder.* = expE{ .LambdaE = .{ .arg = param_name, .body = replace } };
        replace = new_lambder;
    }

    const new_subst = Substitution.init(allocator);
    const var_name = try allocator.dupe(u8, metavar);
    try new_subst.put(var_name, replace);
    return new_subst;
}

pub fn get_arity(exp: *expE) usize {
    var count: usize = 0;
    var cur_exp = exp;

    while (cur_exp.* == .LambdaE) {
        count += 1;
        cur_exp = cur_exp.LambdaE.body;
    }

    return count;
}

pub fn match(allocator: std.mem.Allocator, flexible_term: *expE, rigid_term: *expE, variables: std.StringHashMap(void)) !std.ArrayList(Substitution) {
    var subst = std.ArrayList(Substitution).init(allocator);

    const head = get_head(flexible_term);

    const metavar_name = switch (head.*) {
        .MetavarE => |name| name,
        else => return errors.InvalidFlexibleTerm,
    };
    const rigid_head = get_head(rigid_term);

    const flex_args = try get_args(allocator, flexible_term);
    // defer {
    //     for(flex_args.items) |arg| {
    //         free_exp(allocator, arg, freed_nodes: *std.AutoHashMap(*expE, void))
    //     }
    // }
    const rigid_args = try get_args(rigid_term);

    // Imitation (if rigid head is constant)
    if (is_constant(rigid_head)) {
        const imitation_subst = try create_imitation_subst(allocator, metavar_name, rigid_head, flex_args.items.len, rigid_args.items.len, variables);
        try subst.append(imitation_subst);
    }

    // Projection
    for (0..flex_args.items.len) |i| {
        const arg_arity = 1;

        const projection_subst = try create_projection_subst(allocator, metavar_name, i, flex_args.items.len, arg_arity, variables);
        try subst.append(projection_subst);
    }

    return subst;
}

// pub fn evalStep(allocator: std.mem.Allocator, state: *State) !bool {
//     switch (state.code.*) {
//         .VarE => {
//             std.debug.print("In the VarE portion of Krivine Machine\n", .{});
//             if (state.env.lookup(0)) |closure| {
//                 state.code = closure.exp;
//                 state.env = closure.env;
//             } else {
//                 return errors.UnboundVariableSeen;
//             }
//         },
//         .LambdaE => |lambder| {
//             if (state.stack.pop()) |top| {
//                 std.debug.print("Popping {any} from the stack!\n", .{top});
//                 const closure = try allocator.create(Closure);
//                 closure.* = .{ .exp = top.c, .env = top.oldenv };
//                 const env1 = try Environment.init(allocator, closure, state.env);
//                 state.code = lambder.body;
//                 state.env = env1;
//             } else {
//                 const reduced_expr = try correct_beta_reduce(allocator, lambder.body);
//                 const new_lambda_exp = try allocator.create(expE);
//                 new_lambda_exp.* = .{ .LambdaE = .{ .arg = "", .body = reduced_expr } };
//                 state.code = new_lambda_exp;
//                 std.debug.print("Nothing on the stack. Returning False\n", .{});
//                 return false;
//             }
//         },
//         .ApplyE => |app| {
//             std.debug.print("In the ApplyE portion of Krivine Machine\n", .{});
//             try state.stack.push(.{ .c = app.arg, .oldenv = state.env });
//             state.code = app.func;
//         },
//         .BoundVarE => {
//             if (state.env.lookup(0)) |closure| {
//                 state.code = closure.exp;
//                 state.env = closure.env;
//             } else {
//                 return errors.UnboundVariableSeen;
//             }
//         },
//         .UnboundVarE => {},
//     }
//     return true;
// }
