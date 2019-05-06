(* lexer.mll *)

{
  exception Illegal_character of (Location.t * char)

  type position = [%import: Lexing.position] [@@deriving show]

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    raise (Illegal_character (loc, char))
  
  let () =
    Printexc.register_printer
      (function
        | Illegal_character (loc, char) ->
            Some (Format.asprintf "%a Error: illegal character '%c'"
                    Location.print_loc loc
                    char)
        | _ -> 
            None (* for other exceptions *)
      )
}

let spaces = [' ' '\t']+
let litint = [ '0' - '9']+

rule token = parse
  | spaces        { token lexbuf }
  | '\n'          { token lexbuf }
  | litint as lxm { INT (int_of_string lxm) }
  | eof           { EOF }
  | _             { illegal_character
                      { loc_start = lexbuf.Lexing.lex_start_p;
                        loc_end = lexbuf.Lexing.lex_curr_p
                      }
                      (Lexing.lexeme_char lexbuf 0)
                  }
