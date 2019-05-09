(* location.ml *)

module L = Lexing

(* type for a location in source code *)
type position = [%import: Lexing.position] [@@deriving show]

type t =
  { loc_start: position;
    loc_end: position;
  }
  [@@deriving show]

(* initializing *)

let dummy =
  { loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos;
  }

let curr_loc lexbuf =
  { loc_start = L.lexeme_start_p lexbuf;
    loc_end = L.lexeme_end_p lexbuf;
  }


(* Annotating a value with a location *)

type 'a loc = t * 'a
[@@deriving show]

let mkloc loc x = (loc, x)

let mknoloc x = mkloc dummy x

let loc (location, _) = location


(* Printing *)

let print_pos ppf pos =
  Format.fprintf ppf "%s:%i.%i"
                 pos.L.pos_fname
                 pos.L.pos_lnum
                 (pos.L.pos_cnum - pos.L.pos_bol)

let print_loc ppf loc =
  if loc.loc_start.L.pos_fname = loc.loc_end.L.pos_fname then
    Format.fprintf ppf "%s:%i.%i-%i.%i"
                   loc.loc_start.L.pos_fname
                   loc.loc_start.L.pos_lnum
                   (loc.loc_start.L.pos_cnum - loc.loc_start.L.pos_bol)
                   loc.loc_end.L.pos_lnum
                   (loc.loc_end.L.pos_cnum - loc.loc_end.L.pos_bol)
  else
    Format.fprintf ppf "%a-%a"
                   print_pos loc.loc_start
                   print_pos loc.loc_end
