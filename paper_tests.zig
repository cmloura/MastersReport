const mainfile = @import("test.zig");
const expect = std.testing.expect;
const std = @import("std");
const allocator = std.heap.page_allocator;

test "trivial_example" {
    var dept_dict1 = std.StringHashMap(usize).init(allocator);
    defer dept_dict1.deinit();

    var dept_dict2 = std.StringHashMap(usize).init(allocator);
    defer dept_dict2.deinit();

    const exp1: [*:0]u8 = "lam x. lam y. j";
    const exp2: []u8 = &("lam c. M".*);

    const final_exp1 = (exp1).*;
    const final_exp2 = (exp2).*;

    const tlist1 = try mainfile.scan(final_exp1, allocator);
    const tlist2 = try mainfile.scan(final_exp2, allocator);

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
