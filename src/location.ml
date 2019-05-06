(* location.ml *)

(* type for a location in source code *)

type position = [%import: Lexing.position] [@@deriving show]

type t = { loc_start: position;
           loc_end: position;
         }
[@@deriving show]

(* initializing *)

let in_file name =
  let loc = { Lexing.pos_fname = name;
              Lexing.pos_lnum = 1;
              Lexing.pos_bol = -1;
              Lexing.pos_cnum = 0;
            } in
  { loc_start = loc;
    loc_end = loc;
  }

let none = in_file "_none_"


let lexbuf_ref = ref None

let init lexbuf fname =
  lexbuf_ref := Some lexbuf;
  lexbuf.Lexing.lex_curr_p <- { Lexing.pos_fname = fname;
                                Lexing.pos_lnum = 1;
                                Lexing.pos_bol = -1;
                                Lexing.pos_cnum = 0;
                              }

let curr_loc lexbuf = { loc_start = Lexing.lexeme_start_p lexbuf;
                        loc_end = Lexing.lexeme_end_p lexbuf;
                      }


(* Print a location *)

let print_pos ppf pos =
  Format.fprintf ppf "%s:%i.%i"
                 pos.Lexing.pos_fname
                 pos.Lexing.pos_lnum
                 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let print_loc ppf loc =
  if loc.loc_start.Lexing.pos_fname = loc.loc_end.Lexing.pos_fname then
    Format.fprintf ppf "%s:%i.%i-%i.%i"
                   loc.loc_start.Lexing.pos_fname
                   loc.loc_start.Lexing.pos_lnum
                   (loc.loc_start.Lexing.pos_cnum - loc.loc_start.Lexing.pos_bol)
                   loc.loc_end.Lexing.pos_lnum
                   (loc.loc_end.Lexing.pos_cnum - loc.loc_end.Lexing.pos_bol)
  else
    Format.fprintf ppf "%a-%a"
                   print_pos loc.loc_start
                   print_pos loc.loc_end


(* Annotating a value with a location *)

type 'a loc = t * 'a
[@@deriving show]

let mkloc loc x = (loc, x)

let mknoloc x = mkloc none x

let loc (location, _) = location
