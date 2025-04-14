const std = @import("std");
var FRESH_INDEX: usize = 0;

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    var freed_nodes = std.AutoHashMap(*expE, void).init(allocator);
    var used_vars = std.StringHashMap(void).init(allocator);
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

    // var stack = Stack(StackType).init(allocator);
    // defer stack.deinit();

    //const env = try Environment.init(allocator, null, null);
    //var state = State{ .code = converted_exp, .env = env, .stack = &stack };

    try stdout.print("\n\nCorrect Result for exp1: ", .{});
    const final_exp = try correct_beta_reduce(allocator, converted_exp);
    try print_debruijn_exp(allocator, final_exp);
    //const final_exp2 = try correct_beta_reduce(allocator, second_converted_exp);
    //try stdout.print("\n\nCorrect Result for exp1: ", .{});
    //try print_debruijn_exp(allocator, final_exp2);
    //try print_debruijn_exp(state.code);
    try stdout.print("\n", .{});

    try stdout.print("\nUnification time\n", .{});
    const uni_expr = try copy_expr(allocator, converted_exp);
    const uni_expr2 = try copy_expr(allocator, second_converted_exp);
    try print_debruijn_exp(allocator, uni_expr);
    std.debug.print("\nand\n", .{});
    try print_debruijn_exp(allocator, uni_expr2);

    var unifier = UnifyM.init(allocator, 0);
    const constraint = Constraint.init(uni_expr, uni_expr2);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    if (unification_result) |result| {
        try stdout.print("Unification Successful!\n\n", .{});

        const substitution_map = result.@"0";
        try stdout.print("Substitutions:\n", .{});

        if (substitution_map.count() == 0) {
            try stdout.print("  (No substitutions needed)\n", .{});
        } else {
            var subst_it = substitution_map.iterator();
            while (subst_it.next()) |entry| {
                const metavar = entry.key_ptr.*;
                const replacement = entry.value_ptr.*;

                try stdout.print("  ?{s} => ", .{metavar});
                try print_debruijn_exp(allocator, replacement);
                try stdout.print("\n", .{});
            }
        }

        const remaining_constraints = result.@"1";
        try stdout.print("\nRemaining Constraints:\n", .{});

        if (remaining_constraints.count() == 0) {
            try stdout.print("  (No remaining constraints)\n", .{});
        } else {
            var constraint_it = remaining_constraints.keyIterator();
            var counter: usize = 1;
            while (constraint_it.next()) |key| {
                try stdout.print("  Constraint {d}:\n", .{counter});
                try stdout.print("    Left:  ", .{});
                try print_debruijn_exp(allocator, key.left);
                try stdout.print("\n    Right: ", .{});
                try print_debruijn_exp(allocator, key.right);
                try stdout.print("\n", .{});
                counter += 1;
            }
        }
    } else {
        try stdout.print("Unification Failed: The expressions cannot be unified.\n", .{});
    }

    try free_exp(allocator, expstruct.resexp, &freed_nodes);
    try free_exp(allocator, converted_exp, &freed_nodes);
    // try free_exp(allocator, final_exp, &freed_nodes);
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

const errors = error{ InputEndsButExpectedAnExpression, InputEndsButExpectedToken, TokenSeenButExpected, UnboundVariableSeen, InvalidFlexibleTerm, SubstitutionConflict };

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

pub fn scan(str: []u8, allocator: std.mem.Allocator, used_vars: *std.StringHashMap(void)) ![]Token {
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
const ParseError = error{ InputEndsButExpectedAnExpression, TokenSeenButExpected, AllocatorError, OutOfMemory, InputEndsButExpectedToken, UnboundVariableSeen, InvalidFlexibleTerm, SubstitutionConflict };

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

pub fn exp_equal(e1: *expE, e2: *expE) bool {
    if (@as(std.meta.Tag(expE), e1.*) != @as(std.meta.Tag(expE), e2.*)) {
        return false;
    }

    switch (e1.*) {
        .VarE => |vare| {
            const e2_vare = e2.VarE;
            return std.mem.eql(u8, vare, e2_vare);
        },
        .LambdaE => |lambder| {
            const e2_lambder = e2.LambdaE;
            if (!std.mem.eql(u8, lambder.arg, e2_lambder.arg)) {
                return false;
            }
            return exp_equal(lambder.body, e2_lambder.body);
        },
        .ApplyE => |app| {
            const b_app = e2.ApplyE;
            return exp_equal(app.func, b_app.func) and exp_equal(app.arg, b_app.arg);
        },
        .BoundVarE => |bvare| {
            const e2_bvare = e2.BoundVarE;
            return bvare == e2_bvare;
        },
        .UnboundVarE => |uvare| {
            const e2_uvare = e2.UnboundVarE;
            return std.mem.eql(u8, uvare, e2_uvare);
        },
        .MetavarE => |metavare| {
            const e2_metavare = e2.MetavarE;
            return std.mem.eql(u8, metavare, e2_metavare);
        },
    }
}

pub fn raise_debruijn(allocator: std.mem.Allocator, exp1: *expE, i: usize, lower: usize) !*expE {
    std.debug.print("In raise_debruijn function\n", .{});
    const result = try allocator.create(expE);

    switch (exp1.*) {
        .UnboundVarE => |id| {
            result.* = expE{ .UnboundVarE = id };
        },
        .BoundVarE => |j| {
            if (i > lower) {
                result.* = expE{ .BoundVarE = j + i };
            } else {
                result.* = expE{ .BoundVarE = j };
            }
        },
        .MetavarE => |metavar| {
            result.* = expE{ .MetavarE = metavar };
        },
        .ApplyE => |app| {
            const newleft = try raise_debruijn(allocator, app.func, i, lower);
            const newright = try raise_debruijn(allocator, app.arg, i, lower);
            result.* = expE{ .ApplyE = .{ .func = newleft, .arg = newright } };
        },
        .LambdaE => |lambder| {
            const newbod = try raise_debruijn(allocator, lambder.body, i, lower + 1);
            result.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
        },
        .VarE => |vare| {
            result.* = expE{ .VarE = vare };
        },
    }

    return result;
}

pub fn substitute_boundvar(allocator: std.mem.Allocator, new_term: *expE, i: usize, t: *expE) !*expE {
    std.debug.print("In substitute_boundvar function\n", .{});
    const result = try allocator.create(expE);

    switch (t.*) {
        .UnboundVarE => |id| {
            result.* = expE{ .UnboundVarE = id };
        },
        .BoundVarE => |j| {
            if (j < i) {
                result.* = expE{ .BoundVarE = j };
            } else if (j == i) {
                return try copy_expr(allocator, new_term);
            } else {
                result.* = expE{ .BoundVarE = j - 1 };
            }
        },
        .MetavarE => |metavar| {
            result.* = expE{ .MetavarE = metavar };
        },
        .ApplyE => |app| {
            const newleft = try substitute_boundvar(allocator, new_term, i, app.func);
            const newright = try substitute_boundvar(allocator, new_term, i, app.arg);
            result.* = expE{ .ApplyE = .{ .func = newleft, .arg = newright } };
        },
        .LambdaE => |lambder| {
            const raised_exp = try raise_debruijn(allocator, new_term, 1, 1);
            const newbod = try substitute_boundvar(allocator, raised_exp, i + 1, lambder.body);
            result.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
        },
        .VarE => |vare| {
            result.* = expE{ .VarE = vare };
        },
    }
    return result;
}

pub fn substitute_metavar(allocator: std.mem.Allocator, new_term: *expE, id: []const u8, t: *expE) !*expE {
    std.debug.print("In substitute_metavar function\n", .{});
    const result = try allocator.create(expE);

    switch (t.*) {
        .UnboundVarE => |uvare| {
            result.* = expE{ .UnboundVarE = uvare };
        },
        .BoundVarE => |j| {
            result.* = expE{ .BoundVarE = j };
        },
        .MetavarE => |metavar| {
            if (std.mem.eql(u8, id, metavar)) {
                const copied_exp = try copy_expr(allocator, new_term);
                result.* = copied_exp.*;
            } else {
                result.* = expE{ .MetavarE = metavar };
            }
        },
        .ApplyE => |app| {
            const newleft = try substitute_metavar(allocator, new_term, id, app.func);
            const newright = try substitute_metavar(allocator, new_term, id, app.arg);
            result.* = expE{ .ApplyE = .{ .func = newleft, .arg = newright } };
        },
        .LambdaE => |lambder| {
            const raised = try raise_debruijn(allocator, new_term, 1, 1);
            const newbod = try substitute_metavar(allocator, raised, id, lambder.body);
            result.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
        },
        .VarE => |vare| {
            result.* = expE{ .VarE = vare };
        },
    }
    return result;
}

pub fn substitute_unboundvar(allocator: std.mem.Allocator, new_term: *expE, id: []const u8, t: *expE) !*expE {
    std.debug.print("In substitute_unboundvar function\n", .{});
    const result = try allocator.create(expE);

    switch (t.*) {
        .UnboundVarE => |uvare| {
            if (std.mem.eql(u8, id, uvare)) {
                const copied = try copy_expr(allocator, new_term);
                result.* = copied.*;
            } else {
                result.* = expE{ .UnboundVarE = uvare };
            }
        },
        .BoundVarE => |j| {
            result.* = expE{ .BoundVarE = j };
        },
        .MetavarE => |metavar| {
            result.* = expE{ .MetavarE = metavar };
        },
        .ApplyE => |app| {
            const newleft = try substitute_unboundvar(allocator, new_term, id, app.func);
            const newright = try substitute_unboundvar(allocator, new_term, id, app.arg);
            result.* = expE{ .ApplyE = .{ .func = newleft, .arg = newright } };
        },
        .LambdaE => |lambder| {
            const raised = try raise_debruijn(allocator, new_term, 1, 1);
            const newbod = try substitute_unboundvar(allocator, raised, id, lambder.body);
            result.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = newbod } };
        },
        .VarE => |vare| {
            result.* = expE{ .VarE = vare };
        },
    }
    return result;
}

const MetavarSet = std.StringHashMap(void);
const ConstraintSet = std.AutoHashMap(Constraint, void);
const SubstMap = std.StringHashMap(*expE);

pub fn find_all_metavars(allocator: std.mem.Allocator, term: *expE) !MetavarSet {
    std.debug.print("In find_all_metavars function\n", .{});
    var result = MetavarSet.init(allocator);

    switch (term.*) {
        .VarE, .BoundVarE, .UnboundVarE => {},
        .MetavarE => |metavar| {
            try result.put(metavar, {});
        },
        .ApplyE => |app| {
            var left_set = try find_all_metavars(allocator, app.func);
            defer left_set.deinit();

            var right_set = try find_all_metavars(allocator, app.arg);
            defer right_set.deinit();

            var it = left_set.keyIterator();
            while (it.next()) |key| {
                try result.put(key.*, {});
            }

            var it2 = right_set.keyIterator();
            while (it2.next()) |key| {
                try result.put(key.*, {});
            }
        },
        .LambdaE => |lambder| {
            var body_set = try find_all_metavars(allocator, lambder.body);
            defer body_set.deinit();

            var it = body_set.keyIterator();
            while (it.next()) |key| {
                try result.put(key.*, {});
            }
        },
    }
    return result;
}

pub fn is_closed(expr: *expE) bool {
    std.debug.print("In is_closed function\n", .{});
    switch (expr.*) {
        .UnboundVarE, .VarE => return false,
        .MetavarE, .BoundVarE => return true,
        .ApplyE => |app| {
            return is_closed(app.func) and is_closed(app.arg);
        },
        .LambdaE => |lambder| {
            return is_closed(lambder.body);
        },
    }
}

pub fn reduce(allocator: std.mem.Allocator, expr: *expE) !*expE {
    std.debug.print("In reduce function\n", .{});
    switch (expr.*) {
        .MetavarE, .BoundVarE, .UnboundVarE, .VarE => return expr,
        .ApplyE => |app| {
            const reduced_left = try reduce(allocator, app.func);

            if (reduced_left.* == .LambdaE) {
                const lam_body = reduced_left.*.LambdaE.body;
                const reduced_right = try reduce(allocator, app.arg);
                const right_pointer = try copy_expr(allocator, reduced_right);
                const subst = try substitute_boundvar(allocator, right_pointer, 1, lam_body);
                return try reduce(allocator, subst);
            } else {
                const left_pointer = try copy_expr(allocator, reduced_left);
                const reduced_right = try reduce(allocator, app.arg);

                const result = try allocator.create(expE);
                result.* = expE{ .ApplyE = .{ .func = left_pointer, .arg = reduced_right } };
                return result;
            }
        },
        .LambdaE => |lambder| {
            const reducedbod = try reduce(allocator, lambder.body);
            const result = try allocator.create(expE);
            result.* = expE{ .LambdaE = .{ .arg = lambder.arg, .body = reducedbod } };
            return result;
        },
    }
}

pub fn is_stuck(expr: *expE) bool {
    std.debug.print("In is_stuck function\n", .{});
    switch (expr.*) {
        .MetavarE => return true,
        .ApplyE => |app| return is_stuck(app.func),
        else => return false,
    }
}

pub const ApTelescope = struct {
    head: *expE,
    args: std.ArrayList(*expE),
};

pub fn peepApTelescope(allocator: std.mem.Allocator, expr: *expE) !ApTelescope {
    std.debug.print("In peepApTelescope function\n", .{});
    var result = ApTelescope{ .head = undefined, .args = std.ArrayList(*expE).init(allocator) };

    var current = expr;
    while (true) {
        switch (current.*) {
            .ApplyE => |app| {
                try result.args.append(app.arg);
                current = app.func;
            },
            else => {
                result.head = current;
                break;
            },
        }
    }

    std.mem.reverse(*expE, result.args.items);
    return result;
}

pub fn applyApTelescope(allocator: std.mem.Allocator, head: *expE, args: []const *expE) !*expE {
    std.debug.print("In applyApTelescope function\n", .{});
    const result = head;
    for (args) |arg| {
        var left_pointer = try allocator.create(expE);
        left_pointer = result;

        var right_pointer = try allocator.create(expE);
        right_pointer = arg;
        result.* = expE{ .ApplyE = .{ .func = left_pointer, .arg = right_pointer } };
    }
    return result;
}

pub const Constraint = struct {
    left: *expE,
    right: *expE,

    pub fn init(l: *expE, r: *expE) Constraint {
        return .{ .left = l, .right = r };
    }

    pub fn equals(self: Constraint, other: Constraint) bool {
        return exp_equal(self.left, other.left) and exp_equal(self.right, other.right);
    }
};

pub const UnifyM = struct {
    allocator: std.mem.Allocator,
    next_id: usize,

    pub fn init(all: std.mem.Allocator, start_id: usize) UnifyM {
        return .{ .allocator = all, .next_id = start_id };
    }

    pub fn gen(self: *UnifyM) usize {
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    pub fn simplify(self: *UnifyM, constraint: Constraint) !ConstraintSet {
        std.debug.print("In simplify function\n", .{});
        var result = ConstraintSet.init(self.allocator);

        const t1 = constraint.left;
        const t2 = constraint.right;

        if (exp_equal(t1, t2)) {
            std.debug.print("t1 and t2 are equal!\n", .{});
            var metavar_set = try find_all_metavars(self.allocator, t1);
            defer metavar_set.deinit();

            if (metavar_set.count() == 0) {
                return result;
            }
        }

        const reduced_t1 = try correct_beta_reduce(self.allocator, t1);
        std.debug.print("Reduced t1: ", .{});
        try print_debruijn_exp(self.allocator, reduced_t1);
        std.debug.print("\n", .{});
        if (!exp_equal(reduced_t1, t1)) {
            std.debug.print("TRUE!\n\n", .{});
            return self.simplify(Constraint.init(reduced_t1, t2));
        }

        const reduced_t2 = try correct_beta_reduce(self.allocator, t2);
        std.debug.print("Reduced t2: ", .{});
        try print_debruijn_exp(self.allocator, reduced_t2);
        std.debug.print("\n", .{});
        if (!exp_equal(reduced_t2, t2)) {
            return self.simplify(Constraint.init(t1, reduced_t2));
        }

        const t1_tele = try peepApTelescope(self.allocator, t1);
        defer t1_tele.args.deinit();

        std.debug.print("t1 telescope head: ", .{});
        try print_debruijn_exp(self.allocator, t1_tele.head);
        std.debug.print("\n", .{});
        const t2_tele = try peepApTelescope(self.allocator, t2);
        std.debug.print("t2 telescope head: ", .{});
        try print_debruijn_exp(self.allocator, t2_tele.head);
        std.debug.print("\n", .{});
        defer t2_tele.args.deinit();

        if (t1_tele.head.* == .UnboundVarE and t2_tele.head.* == .UnboundVarE) {
            const i = t1_tele.head.*.UnboundVarE;
            const j = t2_tele.head.*.UnboundVarE;

            if (std.mem.eql(u8, i, j) and t1_tele.args.items.len == t2_tele.args.items.len) {
                for (t1_tele.args.items, t2_tele.args.items) |arg1, arg2| {
                    var sub_constraints = try self.simplify(Constraint.init(arg1, arg2));
                    defer sub_constraints.deinit();

                    var it = sub_constraints.keyIterator();
                    while (it.next()) |key| {
                        try result.put(key.*, {});
                    }
                }
                return result;
            } else {
                return result;
            }
        }

        if (t1.* == .LambdaE and t2.* == .LambdaE) {
            const body1 = t1.*.LambdaE.body;
            const body2 = t2.*.LambdaE.body;

            const fresh_id = self.gen();
            const fresh_var = get_fresh_var(fresh_id, false);
            const var_name = try self.allocator.alloc(u8, 1);
            var_name[0] = fresh_var;

            std.debug.print("Fresh var: {c}\n", .{fresh_var});
            const v = expE{ .UnboundVarE = var_name };
            const v_pointer = try self.allocator.create(expE);
            v_pointer.* = v;

            const subst1 = try substitute_boundvar(self.allocator, v_pointer, 1, body1);
            std.debug.print("subst1 after substituting ", .{});
            try print_debruijn_exp(self.allocator, v_pointer);
            std.debug.print(": ", .{});
            try print_debruijn_exp(self.allocator, subst1);
            std.debug.print("\n", .{});
            const subst2 = try substitute_boundvar(self.allocator, v_pointer, 1, body2);
            std.debug.print("subst2 after substituting ", .{});
            try print_debruijn_exp(self.allocator, v_pointer);
            std.debug.print(": ", .{});
            try print_debruijn_exp(self.allocator, subst2);
            std.debug.print("\n", .{});

            try result.put(Constraint.init(subst1, subst2), {});
            return result;
        }

        if (is_stuck(t1) or is_stuck(t2)) {
            try result.put(constraint, {});
            return result;
        }

        return result;
    }

    pub fn repeatedly_simplify(self: *UnifyM, constraints: ConstraintSet) !ConstraintSet {
        std.debug.print("In repeatedly_simplify function\n", .{});
        var result = ConstraintSet.init(self.allocator);

        var it = constraints.keyIterator();
        while (it.next()) |key| {
            std.debug.print("Top of repeatedly_simplify while loop!\n", .{});
            var simplified = try self.simplify(key.*);
            defer simplified.deinit();

            var sub_it = simplified.keyIterator();
            while (sub_it.next()) |sub_key| {
                std.debug.print("Finished simplifying. Returned constraint: left: ", .{});
                try print_debruijn_exp(self.allocator, sub_key.left);
                std.debug.print(" | right: ", .{});
                try print_debruijn_exp(self.allocator, sub_key.right);
                std.debug.print("\n\n", .{});
                try result.put(sub_key.*, {});
            }
        }

        if (result.count() == constraints.count()) {
            var equal = true;
            var result_it = result.keyIterator();

            while (result_it.next()) |res_key| {
                if (!constraints.contains(res_key.*)) {
                    equal = false;
                    break;
                }
            }

            if (equal) {
                return result;
            }
        }
        return self.repeatedly_simplify(result);
    }

    pub fn apply_substitution(self: *UnifyM, subst: SubstMap, expr: *expE) !*expE {
        std.debug.print("In apply_substitution function\n", .{});
        var result = expr;

        var it = subst.iterator();
        while (it.next()) |entry| {
            const id = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;

            const replacement_pointer = try copy_expr(self.allocator, replacement);

            const substituted = try substitute_metavar(self.allocator, replacement_pointer, id, result);
            result = substituted;
        }

        return result;
    }

    pub fn apply_substitution_to_constraint(self: *UnifyM, subst: SubstMap, constraints: ConstraintSet) !ConstraintSet {
        std.debug.print("In apply_substitution to constrint function\n", .{});
        var result = ConstraintSet.init(self.allocator);

        var it = constraints.keyIterator();
        while (it.next()) |key| {
            const t1 = try self.apply_substitution(subst, key.left);
            std.debug.print("apply_subst_to_constraint t1: ", .{});
            try print_debruijn_exp(self.allocator, t1);
            std.debug.print("\n", .{});
            const t2 = try self.apply_substitution(subst, key.right);
            std.debug.print("apply_subst_to_constraint t1: ", .{});
            try print_debruijn_exp(self.allocator, t2);
            std.debug.print("\n", .{});
            try result.put(Constraint.init(t1, t2), {});
        }
        return result;
    }

    pub fn is_flex_flex(constraint: Constraint) bool {
        return is_stuck(constraint.left) and is_stuck(constraint.right);
    }

    pub fn print_constraint(self: *UnifyM, constraint: Constraint) !void {
        std.debug.print("Left: ", .{});
        try print_debruijn_exp(self.allocator, constraint.left);
        std.debug.print(" | Right: ", .{});
        try print_debruijn_exp(self.allocator, constraint.right);
        std.debug.print("\n\n", .{});
    }

    pub fn try_flex_rigid(self: *UnifyM, constraint: Constraint) !std.ArrayList(SubstMap) {
        std.debug.print("In try_flex_rigid function\n", .{});
        var results = std.ArrayList(SubstMap).init(self.allocator);

        const t1_scope = try peepApTelescope(self.allocator, constraint.left);
        defer t1_scope.args.deinit();

        const t2_scope = try peepApTelescope(self.allocator, constraint.right);
        defer t2_scope.args.deinit();

        if (t1_scope.args.items.len == 0 and t2_scope.args.items.len == 0) {
            var subst = SubstMap.init(self.allocator);
            if (is_stuck(t1_scope.head)) {
                try subst.put(t1_scope.head.MetavarE, t2_scope.head);
            } else {
                try subst.put(t2_scope.head.MetavarE, t1_scope.head);
            }
            try results.append(subst);
            return results;
        }

        if (t1_scope.head.* == .MetavarE and !is_stuck(t2_scope.head)) {
            const mv_id = t1_scope.head.MetavarE;
            const right_metavars = try find_all_metavars(self.allocator, constraint.right);
            if (!right_metavars.contains(mv_id)) {
                const bvars = t1_scope.args.items.len;

                for (0..5) |nargs| {
                    const substs = try self.generate_substitutions(bvars, mv_id, constraint.right, nargs);

                    for (substs.items) |subst| {
                        try results.append(subst);
                    }
                }
            }
        } else if (t2_scope.head.* == .MetavarE and !is_stuck(t1_scope.head)) {
            const mv_id = t2_scope.head.MetavarE;
            const left_mv = try find_all_metavars(self.allocator, constraint.left);

            if (!left_mv.contains(mv_id)) {
                const bvars = t2_scope.args.items.len;

                for (0..5) |nargs| {
                    const substs = try self.generate_substitutions(bvars, mv_id, constraint.left, nargs);
                    for (substs.items) |subst| {
                        try results.append(subst);
                    }
                }
            }
        }
        return results;

        // var new_id: ?[]const u8 = null;
        // var context_len: usize = 0;
        // var stuck_term: *expE = undefined;
        // var is_flex_rigid = false;

        // if (t1_scope.head.* == .MetavarE) {
        //     var metavar_set = try find_all_metavars(self.allocator, constraint.right);
        //     defer metavar_set.deinit();

        //     if (!metavar_set.contains(t1_scope.head.MetavarE)) {
        //         new_id = t1_scope.head.*.MetavarE;
        //         context_len = t1_scope.args.items.len;
        //         stuck_term = t2_scope.head;
        //         //stuck_term = constraint.right;?
        //         is_flex_rigid = true;
        //     }
        // }

        // if (!is_flex_rigid and t2_scope.head.* == .MetavarE) {
        //     new_id = t2_scope.head.*.MetavarE;
        //     context_len = t2_scope.args.items.len;
        //     stuck_term = t1_scope.head;
        //     //stuck_term = constraint.left;
        //     is_flex_rigid = true;
        // }

        // if (!is_flex_rigid) {
        //     return results;
        // }

        // if (context_len == 0) {
        //     var subst = SubstMap.init(self.allocator);
        //     if (is_stuck(t1_scope.head)) {
        //         try subst.put(new_id.?, t2_scope.head);
        //     } else {
        //         try subst.put(new_id.?, t1_scope.head);
        //     }
        //     try results.append(subst);
        //     return results;
        // }

        // const mv = new_id.?;
        // var nargs: usize = 0;
        // while (nargs <= context_len + 3) : (nargs += 1) {
        //     var subst = SubstMap.init(self.allocator);
        //     const inner_mv = get_fresh_var(FRESH_INDEX, true);
        //     FRESH_INDEX += 1;
        //     const metavar_name = try self.allocator.alloc(u8, 1);
        //     metavar_name[0] = inner_mv;
        //     var inner_term = try self.allocator.create(expE);
        //     inner_term.* = expE{ .MetavarE = metavar_name };
        //     var arg_index: usize = 0;
        //     while (arg_index < nargs) : (arg_index += 1) {
        //         const arg = try self.allocator.create(expE);
        //         arg.* = expE{ .BoundVarE = arg_index };
        //         const app = try self.allocator.create(expE);
        //         app.* = expE{ .ApplyE = .{ .func = inner_term, .arg = arg } };
        //         inner_term = app;
        //     }
        //     var i: usize = 0;
        //     while (i < context_len) : (i += 1) {
        //         const arg = try self.allocator.create(expE);
        //         arg.* = expE{ .BoundVarE = i };
        //         const app = try self.allocator.create(expE);
        //         app.* = expE{ .ApplyE = .{ .func = inner_term, .arg = arg } };
        //         inner_term = app;
        //     }

        //     var lambder_bod = inner_term;
        //     var j: usize = 0;
        //     while (j < nargs) : (j += 1) {
        //         const temp = try self.allocator.create(expE);
        //         temp.* = expE{ .LambdaE = .{ .arg = "", .body = lambder_bod } };
        //         lambder_bod = temp;
        //     }
        //     try subst.put(mv, lambder_bod);
        //     try results.append(subst);
        // }

        // if (is_closed(stuck_term)) {
        //     var subst = SubstMap.init(self.allocator);

        //     var term = stuck_term;
        //     var j: usize = 0;
        //     while (j < context_len) : (j += 1) {
        //         const temp = try self.allocator.create(expE);
        //         temp.* = expE{ .LambdaE = .{ .arg = "", .body = term } };
        //         term = temp;
        //     }
        //     try subst.put(mv, term);
        //     try results.append(subst);
        // }
        // return results;
    }

    pub fn generate_substitutions(self: *UnifyM, bvars: usize, mv: []const u8, stuck_term: *expE, nargs: usize) !std.ArrayList(SubstMap) {
        var result = std.ArrayList(SubstMap).init(self.allocator);

        var args = std.ArrayList(*expE).init(self.allocator);

        for (0..nargs) |_| {
            const inner_mv = get_fresh_var(FRESH_INDEX, true);
            FRESH_INDEX += 1;
            const metavar_name = try self.allocator.alloc(u8, 1);
            metavar_name[0] = inner_mv;
            const arg_mv = try self.allocator.create(expE);
            arg_mv.* = expE{ .MetavarE = metavar_name };

            var saturated_mv = arg_mv;
            for (0..bvars) |i| {
                const bvar = try self.allocator.create(expE);
                bvar.* = expE{ .BoundVarE = i };
                const bvar_clone = try copy_expr(self.allocator, bvar);

                const app = try self.allocator.create(expE);
                app.* = expE{ .ApplyE = .{ .func = saturated_mv, .arg = bvar_clone } };
                saturated_mv = app;
            }
            try args.append(saturated_mv);
        }

        for (0..bvars) |i| {
            var subst = SubstMap.init(self.allocator);
            var solution = try self.allocator.create(expE);
            solution.* = expE{ .BoundVarE = i };

            for (0..bvars) |_| {
                const lambder = try self.allocator.create(expE);
                lambder.* = expE{ .LambdaE = .{ .arg = "", .body = solution } };
                solution = lambder;
            }

            if (args.items.len > 0) {
                solution = try applyApTelescope(self.allocator, solution, args.items);
            }

            try subst.put(mv, solution);
            try result.append(subst);
        }

        if (is_closed(stuck_term)) {
            var subst = SubstMap.init(self.allocator);
            var solution = try copy_expr(self.allocator, stuck_term);

            for (0..bvars) |_| {
                const lambder = try self.allocator.create(expE);
                lambder.* = expE{ .LambdaE = .{ .arg = "", .body = solution } };
                solution = lambder;
            }

            if (args.items.len > 0) {
                solution = try applyApTelescope(self.allocator, solution, args.items);
            }

            try subst.put(mv, solution);
            try result.append(subst);
        }
        return result;
    }

    // the <+> operator
    pub fn combine_substitution(self: *UnifyM, s1: SubstMap, s2: SubstMap) !SubstMap {
        std.debug.print("In combine_substitution function\n", .{});
        var result = SubstMap.init(self.allocator);

        // var conflict_it = s1.iterator();
        // while (conflict_it.next()) |entry| {
        //     const id = entry.key_ptr.*;
        //     if (s2.contains(id)) {
        //         return error.SubstitutionConflict;
        //     }
        // }

        var it = s2.iterator();
        while (it.next()) |entry| {
            const id = entry.key_ptr.*;
            const expr = entry.value_ptr;

            const updated_expr = try self.apply_substitution(s1, expr.*);
            try result.put(id, updated_expr);
        }

        it = s1.iterator();
        while (it.next()) |entry| {
            const id = entry.key_ptr.*;
            if (!result.contains(id)) {
                try result.put(id, entry.value_ptr.*);
            }
        }
        return result;
    }

    pub fn unify(self: *UnifyM, subst: SubstMap, constraint: ConstraintSet) !?struct { SubstMap, ConstraintSet } {
        //std.debug.print("In unify function\n", .{});
        var constraint_set = try self.apply_substitution_to_constraint(subst, constraint);
        defer constraint_set.deinit();
        std.debug.print("Number of constraints in constraint_set: {d}\n", .{constraint_set.count()});
        var cs_simplified = try self.repeatedly_simplify(constraint_set);
        defer cs_simplified.deinit();
        std.debug.print("Number of constraints in cs_simplified: {d}\n", .{cs_simplified.count()});
        std.debug.print("Resulting constraints after repeatedly simplifying: [", .{});
        var test_it = cs_simplified.keyIterator();

        while (test_it.next()) |key| {
            try print_debruijn_exp(self.allocator, key.left);
            std.debug.print(", ", .{});
            try print_debruijn_exp(self.allocator, key.right);
            std.debug.print("]\n\n\n", .{});
        }

        var flex_flex = ConstraintSet.init(self.allocator);
        defer flex_flex.deinit();

        var flex_rigid = ConstraintSet.init(self.allocator);
        defer flex_rigid.deinit();

        var it = cs_simplified.keyIterator();
        while (it.next()) |key| {
            std.debug.print("After simplifying in unify method checking constraint: left: ", .{});
            try print_debruijn_exp(self.allocator, key.left);
            std.debug.print(" | right: ", .{});
            try print_debruijn_exp(self.allocator, key.right);
            std.debug.print("\n\n", .{});
            if (is_flex_flex(key.*)) {
                try flex_flex.put(key.*, {});
            } else {
                try flex_rigid.put(key.*, {});
            }
        }

        if (flex_rigid.count() == 0) {
            var result_constraints = ConstraintSet.init(self.allocator);
            var ff_it = flex_flex.keyIterator();
            while (ff_it.next()) |key| {
                try result_constraints.put(key.*, {});
            }
            std.debug.print("Hitting here\n", .{});
            return .{ subst, result_constraints };
        }

        var flex_rigid_it = flex_rigid.keyIterator();
        if (flex_rigid_it.next()) |key| {
            const possible_substs = try self.try_flex_rigid(key.*);
            defer possible_substs.deinit();

            for (possible_substs.items) |new_subst| {
                const combine_subst = try self.combine_substitution(new_subst, subst);
                var all_constraints = ConstraintSet.init(self.allocator);
                var c_it = flex_rigid.keyIterator();
                while (c_it.next()) |c_key| {
                    try all_constraints.put(c_key.*, {});
                }

                c_it = flex_flex.keyIterator();
                while (c_it.next()) |c_key| {
                    try all_constraints.put(c_key.*, {});
                }

                const result = try self.unify(combine_subst, all_constraints);
                if (result != null) {
                    return result;
                }
            }
        }
        return null;
    }

    pub fn start_human_instrumentality(self: *UnifyM, constraint: Constraint) !?struct { SubstMap, ConstraintSet } {
        std.debug.print("Jarvis, start human instrumentality...\n", .{});
        var constraints = ConstraintSet.init(self.allocator);
        defer constraints.deinit();

        try constraints.put(constraint, {});

        const empty_subst = SubstMap.init(self.allocator);
        return try self.unify(empty_subst, constraints);
    }
};

fn get_fresh_var(fresh_id: usize, metavar_flag: bool) u8 {
    std.debug.print("In get_fresh_var function\n", .{});
    if (metavar_flag) {
        const ascii_char = @as(u8, @intCast((fresh_id % 26) + 'A'));
        std.debug.print("ascii_char: {c}\n", .{ascii_char});
        return ascii_char;
    } else {
        const ascii_char = @as(u8, @intCast((fresh_id % 26) + 'a'));
        std.debug.print("ascii_char: {c}\n", .{ascii_char});
        return ascii_char;
    }
}

pub fn generate_fresh_var(allocator: std.mem.Allocator, used_vars: *std.StringHashMap(void)) ![]const u8 {
    var start: u8 = 'A';
    while (start <= 'Z') : (start += 1) {
        const var_name = [_]u8{start};
        const key = var_name[0..1];
        if (!used_vars.contains(key)) {
            const res = try allocator.dupe(u8, key);
            try used_vars.put(res, {});
            return res;
        }
    }

    var i: usize = 1;
    while (true) : (i += 1) {
        var c: u8 = 'A';
        while (c <= 'Z') : (c += 1) {
            const var_name = try std.fmt.allocPrint(allocator, "{c}{d}", .{ c, i });

            if (!used_vars.contains(var_name)) {
                try used_vars.put(var_name, {});
                return var_name;
            }
            allocator.free(var_name);
        }
    }
}
