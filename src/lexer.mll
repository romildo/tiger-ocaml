{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }
}

let spaces = [' ' '\t'] +
let litint = [ '0' - '9'] +

rule token = parse
  | spaces        { token lexbuf }
  | '\n'          { L.new_line lexbuf; token lexbuf }
  | litint as lxm { INT (int_of_string lxm) }
  | eof           { EOF }
  | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
