(* driverl.ml *)

let scan lexbuf =
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n"
      Location.print_pos lexbuf.Lexing.lex_start_p
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  go ()

let main () =
  Printexc.record_backtrace true; (* turns on recording of exception backtraces *)
  Option.parse_cmdline ();
  let lexbuf = Lexing.from_channel (Option.channel ()) in
  Location.init lexbuf (Option.filename ());
  if Option.print_lex () then
    scan lexbuf;
  print_endline "done"

let () = main ()
