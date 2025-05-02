const mainfile = @import("test.zig");
const expect = std.testing.expect;
const std = @import("std");
const allocator = std.heap.page_allocator;

test "trivial_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: []const u8 = "M";
    const exp2: []const u8 = "k";

    const tlist1 = try mainfile.scan(exp1, allocator);
    const tlist2 = try mainfile.scan(exp2, allocator);

    const expstruct = try mainfile.parse_exp(allocator, tlist1);
    const secondexpstruct = try mainfile.parse_exp(allocator, tlist2);
    const converted_exp = try mainfile.convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try mainfile.convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);

    var unifier = mainfile.UnifyM.init(allocator, 0);
    const constraint = mainfile.Constraint.init(converted_exp, second_converted_exp);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    try expect(unification_result != null);
    if (unification_result) |result| {
        const substitution_map = result.@"0";
        var subst_it = substitution_map.iterator();
        while (subst_it.next()) |entry| {
            const metavar = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;
            try expect(std.mem.eql(u8, metavar, "M"));
            const replacement_res = try mainfile.convert_debruijn_exp_to_str(allocator, replacement);
            try expect(std.mem.eql(u8, replacement_res, "k"));
        }
    }
}

test "lambda_removal_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: []const u8 = "lam x. lam y. j";
    const exp2: []const u8 = "lam c. M";

    const tlist1 = try mainfile.scan(exp1, allocator);
    const tlist2 = try mainfile.scan(exp2, allocator);

    const expstruct = try mainfile.parse_exp(allocator, tlist1);
    const secondexpstruct = try mainfile.parse_exp(allocator, tlist2);
    const converted_exp = try mainfile.convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try mainfile.convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);

    var unifier = mainfile.UnifyM.init(allocator, 0);
    const constraint = mainfile.Constraint.init(converted_exp, second_converted_exp);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    try expect(unification_result != null);
    if (unification_result) |result| {
        const substitution_map = result.@"0";
        var subst_it = substitution_map.iterator();
        while (subst_it.next()) |entry| {
            const metavar = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;
            try expect(std.mem.eql(u8, metavar, "M"));
            const replacement_res = try mainfile.convert_debruijn_exp_to_str(allocator, replacement);
            try expect(std.mem.eql(u8, replacement_res, "lam. j"));
        }
    }
}

test "beta_reduction_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: []const u8 = "F";
    const exp2: []const u8 = "(lam x. x y)(lam k. k)";

    const tlist1 = try mainfile.scan(exp1, allocator);
    const tlist2 = try mainfile.scan(exp2, allocator);

    const expstruct = try mainfile.parse_exp(allocator, tlist1);
    const secondexpstruct = try mainfile.parse_exp(allocator, tlist2);
    const converted_exp = try mainfile.convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try mainfile.convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);

    var unifier = mainfile.UnifyM.init(allocator, 0);
    const constraint = mainfile.Constraint.init(converted_exp, second_converted_exp);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    try expect(unification_result != null);
    if (unification_result) |result| {
        const substitution_map = result.@"0";
        var subst_it = substitution_map.iterator();
        while (subst_it.next()) |entry| {
            const metavar = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;
            try expect(std.mem.eql(u8, metavar, "F"));
            const replacement_res = try mainfile.convert_debruijn_exp_to_str(allocator, replacement);
            std.debug.print("Replacement: {s}\n", .{replacement_res});
            try expect(std.mem.eql(u8, replacement_res, "y"));
        }
    }
}

test "involved_lambda_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: []const u8 = "lam c. M (j (M c))";
    const exp2: []const u8 = "lam v. j v";

    const tlist1 = try mainfile.scan(exp1, allocator);
    const tlist2 = try mainfile.scan(exp2, allocator);

    const expstruct = try mainfile.parse_exp(allocator, tlist1);
    const secondexpstruct = try mainfile.parse_exp(allocator, tlist2);
    const converted_exp = try mainfile.convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try mainfile.convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);

    var unifier = mainfile.UnifyM.init(allocator, 0);
    const constraint = mainfile.Constraint.init(converted_exp, second_converted_exp);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    try expect(unification_result != null);
    if (unification_result) |result| {
        const substitution_map = result.@"0";
        var subst_it = substitution_map.iterator();
        while (subst_it.next()) |entry| {
            const metavar = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;
            try expect(std.mem.eql(u8, metavar, "M"));
            const replacement_res = try mainfile.convert_debruijn_exp_to_str(allocator, replacement);
            std.debug.print("Replacement: {s}\n", .{replacement_res});
            try expect(std.mem.eql(u8, replacement_res, "lam. 1"));
        }
    }
}

test "deeper_tree_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: []const u8 = "(lam f. lam g. s) y b";
    const exp2: []const u8 = "(lam a. lam c. a c b) M y";

    const tlist1 = try mainfile.scan(exp1, allocator);
    const tlist2 = try mainfile.scan(exp2, allocator);

    const expstruct = try mainfile.parse_exp(allocator, tlist1);
    const secondexpstruct = try mainfile.parse_exp(allocator, tlist2);
    const converted_exp = try mainfile.convert_debruijn(allocator, expstruct.resexp, &dept_dict1);
    const second_converted_exp = try mainfile.convert_debruijn(allocator, secondexpstruct.resexp, &dept_dict2);

    var unifier = mainfile.UnifyM.init(allocator, 0);
    const constraint = mainfile.Constraint.init(converted_exp, second_converted_exp);
    const unification_result = try unifier.start_human_instrumentality(constraint);

    try expect(unification_result != null);
    if (unification_result) |result| {
        const substitution_map = result.@"0";
        var subst_it = substitution_map.iterator();
        while (subst_it.next()) |entry| {
            const metavar = entry.key_ptr.*;
            const replacement = entry.value_ptr.*;
            try expect(std.mem.eql(u8, metavar, "M"));
            const replacement_res = try mainfile.convert_debruijn_exp_to_str(allocator, replacement);
            std.debug.print("Replacement: {s}\n", .{replacement_res});
            try expect(std.mem.eql(u8, replacement_res, "lam. lam. s"));
        }
    }
}
