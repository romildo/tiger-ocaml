(* symbol.ml *)

type symbol = string * int
[@@deriving show]

let nextsym = ref 0

let size_hint = 128

let hashtable = Hashtbl.create size_hint

let name (s,_) = s

let symbol name =
  try
    let i = Hashtbl.find hashtable name in
    (name,i)
  with Not_found ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name,i)

let new_symbol s = symbol (Printf.sprintf "%s_%d" s !nextsym)

let pp_symbol ppf s =
  Format.fprintf ppf "\"%s\"" (name s)


module Table = Map.Make(
  (* purely applicative (no side-effects) association tables over ordered types *)
  struct
    type t = symbol
    let compare (_,n1) (_,n2) = compare n1 n2
  end
  )

type 'a table = 'a Table.t

let empty = Table.empty
let enter = Table.add
(* let look = Table.find *)
let look k t =
  try Some (Table.find k t)
  with Not_found -> None
