type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal

(* Function 1: returns true iff the set represented by list a is a subset of
set represented by list  b*)
(*The function should be generic to lists of any type*)

let rec subset a b = match a with
|[]->true
|h1::t1 ->if List.mem (List.hd a) b
then subset (List.tl a) b
else false
;;

(* Function 2: returns true iff the represented sets are equal*)
let equal_sets a b = subset a b && subset b a
;;

(* Function 3 *)
let set_union a b = List.merge compare a b
;;


(* Function 4 *)

let set_intersection a b = match a with
| [] -> []
| h1::t1 -> List.find_all (fun ele -> (List.mem ele a)) b
;;

(* Function 5 - Returns a list representing a−b *)
let set_diff a b = match a with
| [] -> []
| h1::t1 -> List.find_all (fun ele -> not (List.mem ele b)) a
;;


(* Function 6 *)
let rec computed_fixed_point eq f x =
let temp = f x in
if eq temp x
then x
else computed_fixed_point eq f temp
;;

(* Function 7 -  returns a copy of the grammar g with all unreachable rules
removed. *)
(* Use equal_sets and computed_fixed_point *)

let filter_reachable g = match g with
| (start_symbol, rules)->
		let first (el1, _) = el1 in
		let second (_, el2) = el2 in
		let reach_symbol (a, b) =
							let rec keep_nonterm a = match a with
								| [] -> []
								| N h1::t1 -> h1::(keep_nonterm t1)
								| T h1::t1 -> keep_nonterm t1
								in
		          let rec helper (a, b) = match (a, b) with
		                    | ([], b) -> (a, b)
		                    | _ -> if List.mem (first (List.hd a)) b
												then helper ((List.tl a), (List.append b (keep_nonterm
												(second (List.hd a)))))
		                    else helper ((List.tl a), b)
		                    in
		                    (a, second (helper (a, b)))
		in
		let checksecond el1 el2 = equal_sets (second el1) (second el2) in
		let (a,b) = computed_fixed_point checksecond reach_symbol (rules,[start_symbol]) in
		(start_symbol, List.find_all (fun x -> List.mem (first x) b ) rules)
		;;
