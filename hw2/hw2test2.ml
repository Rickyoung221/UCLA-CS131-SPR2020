let accept_all string = Some string

type nonterminals =
	| Start | Operator | Grouping | Variable

let empty_grammar =
	(Start,
	function
		| Start ->
			[[N Grouping];
			[N Grouping; N Operator; N Start];
			[]]
		| Grouping ->
			[[N Variable];
			[T "("; N Start; T ")"];
			[]]
		| Operator ->
			[[T "-"];
			[T "+"];
			[T "*"];
			[T "/"]]
		| Variable ->
			[[T "$"; N Start]])

let make_matcher_test = (make_matcher empty_grammar accept_all ["(";"+";"$";")";"+";"(";")"]) = Some ["+";"(";")"];;
let extract_some input =
	match input with
		| Some value -> value
		| None -> Leaf "i";;
let make_parser_test = (parse_tree_leaves (extract_some (make_parser empty_grammar ["(";"+";"$";")";"+";"(";")"])) = ["(";"+";"$";")";"+";"(";")"]);;
