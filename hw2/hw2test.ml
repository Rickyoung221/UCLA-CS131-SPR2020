let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type my_nonterminals =
| Binary | AZ

let my_grammar =
  (Binary,
  function
      | Binary ->
        [[T "1"; T "0"];
         [T "1"; T "0"; N AZ];
         [T "1"; N Binary; T "0"]]
      | AZ ->
        [[T "A"; T "Z"];
         [T "A"; N AZ; T "Z"];
         [T "A"; T "Z"; N Binary]])

let frag = [ "1"; "1"; "A" ; "Z"; "Z"; "1"; "0" ]

let make_matcher_test =
  ((make_matcher my_grammar accept_all frag) = Some []);;

let make_parser_test = ((make_parser my_grammar frag)
= Some (Node (Binary,
            [Leaf "1";
            Node (Binary,
                [Leaf "1"; Leaf "0";
            Node (AZ,
                [Leaf "A"; Leaf "Z";
            Node (Binary,
                [Leaf "1"; Leaf "0"]
                )])]);
            Leaf "0"])))
