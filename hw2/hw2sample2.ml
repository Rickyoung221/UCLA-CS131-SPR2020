type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awksub_rules =
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

let awksub_grammar = Expr, awksub_rules

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
   [[N Num];
    [N Lvalue];
    [N Incrop; N Lvalue];
    [N Lvalue; N Incrop];
    [T"("; N Expr; T")"]]
     | Lvalue ->
   [[T"$"; N Expr]]
     | Incrop ->
   [[T"++"];
    [T"--"]]
     | Binop ->
   [[T"+"];
    [T"-"]]
     | Num ->
   [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
    [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((make_matcher awkish_grammar accept_all ["ouch"]) = None)

let test1 =
  ((make_matcher awkish_grammar accept_all ["9"])
   = Some [])

let test2 =
  ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some ["+"])

let test3 =
  ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some [])

let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])


let test_custom1 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"])
  = Some [])

let test_custom2 =
 ((make_matcher awkish_grammar accept_all
     ["(";"++";"$";"5";")"])
  = Some [])

let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let test_custom3 =
  ((make_matcher awkish_grammar accept_all small_awk_frag)
  = Some [])

let test6 =
  ((make_parser awkish_grammar small_awk_frag)
   = Some (Node (Expr,
    [Node (Term,
      [Node (Lvalue,
             [Leaf "$";
        Node (Expr,
              [Node (Term,
               [Node (Num,
                [Leaf "1"])])])]);
       Node (Incrop, [Leaf "++"])]);
      Node (Binop,
      [Leaf "-"]);
      Node (Expr,
      [Node (Term,
             [Node (Num,
              [Leaf "2"])])])])))

let test7 =
  match make_parser awkish_grammar small_awk_frag with
    | Some tree -> parse_tree_leaves tree = small_awk_frag
    | _ -> false

let frag1 = ["3"]
let frag2 = ["("; "$"; "3"; "++"; ")"]
let frag3 = ["1"; "+"; "2"; "+"; "5"; "+"; "7"; "+"; "9"; "-";
             "("; "--"; "$"; "8"; "+"; "$"; "6"; "--"; "-"; "++";
             "$"; "4"; "+"; "$"; "3"; "++"; ")"]

let test_suit grammar frag =
  match make_parser grammar frag with
    | Some tree -> parse_tree_leaves tree = frag
    | _ -> false

let test_custom4 = test_suit awkish_grammar frag1
let test_custom5 = test_suit awkish_grammar frag2
let test_custom6 = test_suit awkish_grammar frag3

(* Grammar of all palindromes with letters "a" and "b". *)
type custom_nonterminals = S

let custom_grammar =
  (S, function
    | S -> [[T "a"; N S; T "a"];
            [T "b"; N S; T "b"];
            [T "a"];
            [T "b"];
            []])


let accept_only_a frag =
  let rec check = function
    | [] -> true
    | "a"::tail -> check tail
    | _ -> false
  in
  if check frag then Some frag
  else None

let make_matcher_test =
  (((make_matcher custom_grammar accept_all ["a";"b";"b";"a";"a"]) = Some ["a"]) &&
  ((make_matcher custom_grammar accept_only_a ["a";"a";"a";"a";"b";"a";"a"]) = None) &&
  ((make_matcher custom_grammar accept_all ["a";"a";"a";"a";"b";"a";"a"])
      = Some ["b"; "a"; "a"]) &&
  ((make_matcher custom_grammar accept_empty_suffix ["a";"b";"a"]) = Some []) &&
  ((make_matcher custom_grammar accept_empty_suffix ["a";"b";"b";"a"]) = Some []))

let make_parser_test =
  ((test_suit custom_grammar ["a";"b";"a";"b";"b";"b";"b";"a";"b";"a"]) &&
  (test_suit custom_grammar []) &&
  (make_parser custom_grammar ["a";"a";"b";"a";"b";"a"] = None) &&
  (make_parser custom_grammar ["a";"b"] = None))
