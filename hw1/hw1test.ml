let my_subset_test0 = subset [1] [1;3]
let my_subset_test1 = subset [2] [1;3]

let my_equal_sets_test0 = equal_sets [] [2;4;6]
let my_equal_sets_test1 = equal_sets [2;4;6] [2;4;6]

let my_set_union_test0 = equal_sets (set_union [] [2;4;6]) [2;4;6]
let my_set_union_test1 = equal_sets (set_union [1;2] [3;4]) [1;2;3;4]

let my_set_intersection_test0 = equal_sets (set_intersection [2;5] [2;4;6]) [2]
let my_set_intersection_test1 = not (equal_sets (set_intersection [] [2;4;6]) [])

let my_set_diff_test0 = equal_sets (set_diff [5;6;7;8;9] [6;7]) [5;8;9]
let my_set_diff_test1 = equal_sets (set_diff [5;6;7;8;9] []) [5;6;7;8;9]

let my_computed_fixed_point_test0 =  computed_fixed_point (=) (fun x -> x ) 0 = 0

type sample_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let sample_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let my_filter_reachable_test0 =
filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let my_filter_reachable_test1 =
filter_reachable (Incrop, awksub_rules) = (Incrop, awksub_rules)
