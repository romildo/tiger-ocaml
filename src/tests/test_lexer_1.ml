open OUnit2

module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n"
      Location.print_loc (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  go ()

let lex_string s =
  let lexbuf = L.from_string s in
  let rec go tokens =
    let tok = Lexer.token lexbuf in
    let tokens' = tok :: tokens in
    match tok with
    | Parser.EOF -> List.rev tokens'
    | _ -> go tokens'
  in
  go []

let rec show_list ?(separator=" ") show_element = function
  | [] -> ""
  | h :: t -> show_element h ^ separator ^ show_list ~separator show_element t

(* Name the test cases and group them together *)
let suite =
  "lexical analysis" >::: [
    "test1" >:: (fun _ -> assert_equal (scan_string "10") ());
    "test2" >:: (fun _ -> assert_equal ~printer:(show_list Lexer.show_token) (lex_string "2041") [Parser.INT 247; Parser.EOF])
  ]

let () =
  run_test_tt_main suite
