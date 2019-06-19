(* types.ml *)

type unique = unit ref
[@@deriving show]

type ty =
  | UNIT
  | INT
  | STRING
  | NIL
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | FUNCTION of ty list * ty
  | NAME of Symbol.symbol * ty option ref
[@@deriving show]

let rec actual_ty = function
  | NAME (_,{contents=Some t}) -> actual_ty t
  | t                          -> t

let rec coerceable a b =
  match a,b with
  | NAME (_,{contents=Some t}), b                          -> coerceable t b
  | a                         , NAME (_,{contents=Some t}) -> coerceable a t
  | UNIT                      , UNIT                       -> true
  | INT                       , INT                        -> true
  | STRING                    , STRING                     -> true
  | NIL                       , NIL                        -> true
  | NIL                       , RECORD _                   -> true
  | RECORD (_,uniqa)          , RECORD (_,uniqb)           -> uniqa = uniqb
  | ARRAY (_,uniqa)           , ARRAY (_,uniqb)            -> uniqa = uniqb
  | FUNCTION _                , FUNCTION _                 -> false
  | _                                                      -> false


let name = Symbol.name
let map = List.map
let mkt = Tree.mkt

let rec string_of_ty = function
  | UNIT                      -> "UNIT"
  | INT                       -> "INT"
  | STRING                    -> "STRING"
  | NIL                       -> "NIL"
  | RECORD _                  -> "RECORD"
  | ARRAY _                   -> "ARRAY"
  | FUNCTION (formals,result) -> "FUNCTION:" ^ String.concat "->" (List.map string_of_ty (formals @ [result]))
  | NAME (n,_)                -> "NAME " ^ name n

let rec tree_of_ty = function
  | UNIT                      -> mkt "UNIT" []
  | INT                       -> mkt "INT" []
  | STRING                    -> mkt "STRING" []
  | NIL                       -> mkt "NIL" []
  | RECORD (fs,_)             -> mkt "RECORD" (map (fun (x,t) -> mkt "Field" [mkt (name x) []; tree_of_ty t]) fs)
  | ARRAY (t,_)               -> mkt "ARRAY" [tree_of_ty t]
  | FUNCTION (formals,result) -> mkt "FUNCTION" [mkt "Args" (map tree_of_ty formals); mkt "Result" [tree_of_ty result]]
  | NAME (n,{contents=mt})    -> mkt ("NAME " ^ name n) (match mt with
                                                         | None -> []
                                                         | Some t -> [tree_of_ty t])
