module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
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

let (>::) title input =
  Printf.printf "=======================================\n";
  Printf.printf "test: %s\n" title;
  Printf.printf "---------------------------------------\n";
  print_endline input;
  Printf.printf "---------------------------------------\n";
  scan_string input;
  Printf.printf "%!"

let test () =
  "eof"              >:: "";
  "spaces"           >:: "  \t\n\n\n  ";
  "integer literals" >:: "0 1342 -56 +32 98fim 139015.007";
  "string literals"  >:: {|"text" "abc\tEND!"|};
  "string literals"  >:: "\"tab \\t; nl \\n; quote \\\"; slash \\\\; control i \^I; 065 \065; empty \\  \t \\;\"";
  "identifier"       >:: "altura";
  "comment"          >:: "/* ignore me */";
  "nested comments"  >:: "/* level 1\n /* level 2\n  /* level 3 */ level 2 /* level 3 */\n level 2 */ level 1 */"(*);
  "unterminated comment" >:: "/* level 1 /* level 2 /* level 3 */ level 2 /* level 3 */" *)

let () = test ()
