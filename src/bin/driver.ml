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
    try
      let ast = Parser.program Lexer.token lexbuf in
      print_endline "Abstract syntax tree:";
      print_endline "============================================================";
      (* Format.printf "%a\n" Absyn.pp_lexp ast; *)
      print_endline (Tree.string_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast)));
      print_newline ();
      (* print_endline (Box.string_of_box (Tree.box_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast))));
      print_newline (); *)
      let dotchannel = open_out "ast.dot" in
      output_string dotchannel (Tree.dot_of_tree "AST" (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast)));
      print_endline "Semantic analysis:";
      print_endline "============================================================";
      let t = Semantic.type_check ast in
      print_endline (Types.show_ty t)
    with
    | Parser.Error ->
      Format.printf "%a error: syntax\n" Location.pp_position lexbuf.L.lex_curr_p;
      exit (1)
    | Error.Error (loc, msg) ->
      Format.printf "%a error: %s" Location.pp_location loc msg;
      exit (2)

let () = main ()
