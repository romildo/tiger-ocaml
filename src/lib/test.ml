module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  go ()

let%expect_test _ =
  scan_string "";
    [%expect{| :1.0-1.0 Parser.EOF |}];
  scan_string "   \t\t \n     \n  ";
    [%expect{| :3.2-3.2 Parser.EOF |}];
  scan_string "0 1342 -56 +32 98fim 139015.007";
    [%expect{|
      :1.0-1.1 (Parser.INT 0)
      :1.2-1.6 (Parser.INT 1342)
      :1.7-1.8 Parser.MINUS
      :1.8-1.10 (Parser.INT 56)
      :1.11-1.12 Parser.PLUS
      :1.12-1.14 (Parser.INT 32)
      :1.15-1.17 (Parser.INT 98)
      :1.17-1.20 (Parser.ID "fim")
      :1.21-1.27 (Parser.INT 139015)
      :1.27-1.28 Parser.DOT
      :1.28-1.31 (Parser.INT 7)
      :1.31-1.31 Parser.EOF |}];
  scan_string "abc";
    [%expect{|
      :1.0-1.3 (Parser.ID "abc")
      :1.3-1.3 Parser.EOF
    |}]
