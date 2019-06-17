(* driverl.ml *)

module L = Lexing

let scan lexbuf =
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  go ()

let main () =
  Printexc.record_backtrace true; (* turns on recording of exception backtraces *)
  Option.parse_cmdline ();
  let lexbuf = L.from_channel (Option.channel ()) in
  Lexer.set_filename lexbuf (Option.filename ());
  if Option.print_lex () then
    scan lexbuf
  else if Option.print_parse () then
  (
    try
      let ast = Parser.program Lexer.token lexbuf in
      Format.printf "%a\n" Absyn.pp_lexp ast
    with
    | Parser.Error ->
      Format.printf "%a error: syntax\n" Location.pp_position lexbuf.L.lex_curr_p;
      exit (1)
    | Error.Error (loc, msg) ->
      Format.printf "%a error: %s" Location.pp_location loc msg;
      exit (2)
  );
  print_endline "done"

let () = main ()
