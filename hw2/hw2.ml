type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* function 1 *)
(* That function should be able to give us the corresponding alternative list
whenever it sees an nonterminal symbol. *)
(* helper function to convert the rhs to production function *)

let rec production nt rules = match rules with
| [] -> []
| hd::tl -> if ( fst hd = nt)			(* fst: first element of the pair *)
		then (snd hd)::(production nt tl)
		else production nt tl
;;

let convert_grammar gram1 = match gram1 with
| (symbol, rulelists) -> (symbol, fun x -> production x rulelists)
;;

(* function 2 *)
type ('nonterminal, 'terminal) parse_tree =
        | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
        | Leaf of 'terminal

let rec tree_helper tree = match tree with
| [] -> []
| hd::tl -> match hd with
	| Node (nt, rest) -> List.append (tree_helper rest) (tree_helper tl)   (* remove the node *)
	| Leaf leaf -> leaf::(tree_helper tl)
;;

(* tree is a list *)
let parse_tree_leaves tree = tree_helper [tree]
;;

(* function 3 *)

(* returns a matcher function *)
(* matcher accept frag tries to find a matching prefix from fragment frag and calls
accept to check whether the suffix should be accepted *)
(* Note that due to currying/partial application, make_matcher does not
really need to return a function; it can also be a function that takes more than one
input argument *)

(* Function from dis *)
(* Always fail, i.e., never accept a match.  *)
let accept_none accept frag = None
(* Always succeed.  This acceptor returns the suffix
   containing all the symbols that were not matched.
   It causes the matcher to return the unmatched suffix.  *)
let accept_all x = Some x
(* Accept only the empty fragment.  *)
let accept_empty accept frag = accept frag
;;


let rec matcher gram rule_list  = fun accept frag ->
match rule_list with
| [] -> accept_none accept frag   (* no acceptable match is found, return none *)
| rule_hd::rules_tl ->
	let tl_match = matcher gram rules_tl accept frag in
	let hd_match = retrieve gram rule_hd accept frag in
		match hd_match with
		| None -> tl_match
		| _ -> hd_match
		(* mathced and retrun acceptor output *)
and
retrieve gram rule_hd = fun accept frag ->
match frag with
| [] -> (match rule_hd with
			| [] -> accept_empty accept frag
			| _ -> accept_none accept frag)
(* if rule_hd = [] then (accept_empty accept frag) else (accept_none accept frag) *)
| hd::tl -> match rule_hd with
	| [] -> accept_empty accept frag
	| (T terminal)::tl1 ->
		if (terminal = hd) then (retrieve gram tl1 accept tl)
		else (accept_none accept frag)
		(* returns a matcher *)
	| (N nonterminal)::tl2 -> let new_accept = retrieve gram tl2 accept in
		(matcher gram ((snd gram) nonterminal))
		new_accept frag ;;

let make_matcher gram =
        let start =(fst gram) in
        let rule = (snd gram) in
 fun accept frag -> matcher gram (rule start) accept frag
;;




(* Function 4 *)

let accept_empty_suffix = function
   | [] -> Some []
   | _  -> None
	 ;;

let rec parser gram rule_list  = fun accept frag ->
match rule_list with
| [] -> accept_none accept frag   (* no acceptable match is found, return none *)
| rule_hd::rules_tl ->
	let tl_match = parser gram rules_tl accept frag in
	let hd_match = check_rule gram rule_hd accept frag in
		match hd_match with
		| None -> tl_match
		| Some x -> accept_all (rule_hd::x)
		(* mathced and retrun acceptor output *)
and
check_rule gram rule_hd = fun accept frag ->
match frag with
| [] -> (match rule_hd with
			| [] -> accept_empty accept frag
			| _ -> accept_none accept frag)
| hd::tl -> match rule_hd with
	| [] -> accept_empty accept frag
	| (T terminal)::tl1 ->
		if (terminal = hd) then (check_rule gram tl1 accept tl)
		else (accept_none accept frag)
		(* returns a matcher *)
	| (N nonterminal)::tl2 -> let new_accept = check_rule gram tl2 accept in
		(parser gram ((snd gram) nonterminal)) new_accept frag
		;;

(*note that children arguement should be passed as [] at the beginning *)
		let rec rhs2children rhs children = match rhs with
			 		| [] -> children     (* return children after the python for loop in rhs ends*)
			 		| hd::tl -> match hd with
			 						| T terminal -> rhs2children tl (List.append children [Leaf terminal])
			 						| N nonterminal -> rhs2children tl (List.append children [Node (nonterminal, [])])
		;;

(* From the python hint code *)
let rec construct_tree rhs_traced temp_root = match rhs_traced with
	| [] -> (rhs_traced, temp_root)
	|hd_rhs::tl -> match temp_root with
	(* equivaluent to len(rhs_traced) > 0, hd = rhs_traced[0] tl = rhs_traced[1:] *)
	(* If this is a node, need to expand its child and remove one rule from the traced list *)
		| Leaf leaf -> (rhs_traced, temp_root) (* final result *)
		(* If this is a node, need to expand its child and remove one rule from the traced list *)
		| Node (nt, rest) ->      (* if temp_root is node *)
		(* let temp_rhs = tl in *)
			let root_children = rhs2children hd_rhs [] in
		 (* temp_root.children = rhs2children(tmp_rhs) *)
			let temp_children, new_rhs = loop_helper root_children tl in
			let new_root = Node (nt, temp_children) in (new_rhs, new_root)
and
loop_helper root_children temp_rhs = match root_children with
	| [] -> ([], temp_rhs)   (* return when the for loop ends for len(tmp_root.children) *)
	| h1::t1 -> let temp = construct_tree temp_rhs h1 in
	(* temp: constructing_tree (rhs_traced, temp_root.children[i]) *)
		match temp with
		| (new_rhs, rest) -> let next_loop = loop_helper t1 new_rhs in
    		match next_loop with (* keep loop *)
				| (new_children, new_rhs) -> (rest::new_children, new_rhs)
;;



let make_parser gram = fun frag ->
	let start =(fst gram) in
	let rule = (snd gram) in
				match frag with
				| [] -> None
				| _ -> match parser gram (rule start) accept_empty_suffix frag with
							| Some [] -> None
							| None -> None
							| Some rhs -> match construct_tree rhs (Node (start, [])) with
								| ([], new_root) -> Some new_root
								|_ -> None
				;;
