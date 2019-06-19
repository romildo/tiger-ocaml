{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence: %s" sequence

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }

  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str

  let append_char str ch =
    str ^ (String.make 1 (Char.chr ch))
}

let spaces = [' ' '\t'] +
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let litint = digit +
let id = alpha+ (alpha | digit | '_')*

rule token = parse
  | spaces        { token lexbuf }
  | '\n'          { L.new_line lexbuf; token lexbuf }
  | "/*"          { comment [lexbuf.L.lex_start_p] lexbuf }
  | litint as lxm { INT (int_of_string lxm) }
  | '"'           { string lexbuf.L.lex_start_p "" lexbuf }
  | "array"       { ARRAY }
  | "break"       { BREAK }
  | "do"          { DO }
  | "else"        { ELSE }
  | "end"         { END }
  | "for"         { FOR }
  | "function"    { FUNCTION }
  | "if"          { IF }
  | "in"          { IN }
  | "let"         { LET }
  | "nil"         { NIL }
  | "of"          { OF }
  | "then"        { THEN }
  | "to"          { TO }
  | "type"        { TYPE }
  | "var"         { VAR }
  | "while"       { WHILE }
  | id as lxm     { ID (Symbol.symbol lxm) }
  | ":="          { ASSIGN }
  | '|'           { OR }
  | '&'           { AND }
  | '='           { EQ }
  | "<>"          { NE }
  | '<'           { LT }
  | "<="          { LE }
  | '>'           { GT }
  | ">="          { GE }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACK }
  | ']'           { RBRACK }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | '.'           { DOT }
  | ':'           { COLON }
  | ','           { COMMA }
  | ';'           { SEMI }
  | eof           { EOF }
  | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }

and comment level = parse
  | "*/" { match level with
           | [_] -> token lexbuf
           | _::level' -> comment level' lexbuf
           | [] -> failwith "bug in comment scanner"
         }
  | "/*" { comment (lexbuf.L.lex_start_p :: level) lexbuf }
  | '\n' { L.new_line lexbuf;
           comment level lexbuf
         }
  | _    { comment level lexbuf }
  | eof  { unterminated_comment (List.hd level, lexbuf.L.lex_start_p);
           token lexbuf
         }

and string pos buf = parse
  | '"'                               { lexbuf.L.lex_start_p <- pos;
                                        STR buf
                                      }
  | "\\n"                             { string pos (buf ^ "\n") lexbuf }
  | "\\t"                             { string pos (buf ^ "\t") lexbuf }
  | "\\\""                            { string pos (buf ^ "\"") lexbuf }
  | "\\\\"                            { string pos (buf ^ "\\") lexbuf }
  | "\\^" (['@' 'A'-'Z'] as x)        { string pos (append_char buf (Char.code x - Char.code '@')) lexbuf }
  | "\\^" (['a'-'z'] as x)            { string pos (append_char buf (Char.code x - Char.code 'a' + 1)) lexbuf }
  | "\\" (digit digit digit as x)     { string pos (append_char buf (int_of_string x)) lexbuf }
  | "\\" ([' ' '\t' '\n']+ as x) "\\" { str_incr_linenum x lexbuf;
                                        string pos buf lexbuf
                                      }
  | "\\" ([' ' '\t' '\n']+ as x) "\\" { str_incr_linenum x lexbuf;
                                        string pos buf lexbuf
                                      }
  | "\\" _ as x                       { illegal_escape (lexbuf.L.lex_start_p, lexbuf.L.lex_curr_p) x;
                                        string pos buf lexbuf
                                      }
  | [^ '\\' '"']+ as x                { str_incr_linenum x lexbuf;
                                        string pos (buf ^ x) lexbuf
                                      }
  | eof                               { unterminated_string (pos, lexbuf.L.lex_start_p);
                                        token lexbuf
                                      }
