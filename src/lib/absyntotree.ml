open Absyn

let name = Symbol.name
let map  = List.map
let mkt s = Tree.mkt [s]

let string_of_oper = function
  | PlusOp   -> "+"
  | MinusOp  -> "-"
  | TimesOp  -> "*"
  | DivideOp -> "/"
  | EqOp     -> "="
  | NeOp     -> "<>"
  | LtOp     -> "<"
  | LeOp     -> "<="
  | GtOp     -> ">"
  | GeOp     -> ">="
  | AndOp    -> "&"
  | OrOp     -> "|"

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
    | [] -> Bytes.to_string res
    | c :: l -> Bytes.set res i c; imp (i + 1) l in
  imp 0 l

let escape_dot s =
  implode
    (List.fold_right
       (fun c cs ->
         match c with
           | '\n' -> '\\' :: 'n' :: cs
           | '<' | '{' | '|' | '}' | '>' | ' ' -> '\\' :: c :: cs
           | _ -> c :: cs
       )
       (explode s)
       [])

let node_txt xs = String.concat ":" xs

let node_dot s =
  let node_dot = function
    | [] -> ""
    | [x] -> escape_dot x
    | xs -> "{ " ^ String.concat " | " (List.map escape_dot xs) ^ " }"
  in
  let s' = node_dot s in
  (* print_string s'; *)
  s'

let root s =
  [s]

let rec tree_of_exp exp =
  let mktr s = Tree.mkt (root s) in
  match exp with
  | NilExp             -> mktr "NilExp" []
  | IntExp x           -> mktr ("IntExp " ^ string_of_int x) []
  | StringExp x        -> mktr ("StringExp " ^ x) []
  | ArrayExp (t,s,e)   -> mktr "ArrayExp" [mkt (name t) []; tree_of_lexp s; tree_of_lexp e]
  | RecordExp (t,fs)   -> mktr "RecordExp"
                              [ mkt (name t) []
                              ; mkt "Fields" (map
                                                (fun (_,(n,e)) ->
                                                  mkt "Field" [ mkt (name n) []
                                                              ; tree_of_lexp e
                                                              ])
                                                fs)
                              ]
  | VarExp v           -> mktr "VarExp" [tree_of_lvar v]
  | AssignExp (v,e)    -> mktr "AssignExp" [tree_of_lvar v; tree_of_lexp e]
  | CallExp (f,xs)     -> mktr "CallExp" [mkt (name f) []; mkt "Args" (map tree_of_lexp xs)]
  | OpExp (op,e1,e2)   -> mktr ("OpExp " ^ string_of_oper op) [tree_of_lexp e1; tree_of_lexp e2]
  | IfExp (cond,e1,e2) -> mktr "IfExp" [tree_of_lexp cond; tree_of_lexp e1; match e2 with None -> mkt "" [] | Some k -> tree_of_lexp k]
  | WhileExp (cond,e)  -> mktr "WhileExp" [tree_of_lexp cond; tree_of_lexp e]
  | ForExp (v,lo,hi,e) -> mktr "ForExp" [mkt (name v) []; tree_of_lexp lo; tree_of_lexp hi; tree_of_lexp e]
  | BreakExp           -> mktr "BreakExp" []
  | SeqExp es          -> mktr "SeqExp" (map tree_of_lexp es)
  | LetExp (ds,e)      -> mktr "LetExp" [mkt "Decls" (map tree_of_ldec ds); tree_of_lexp e]

and tree_of_var = function
  | SimpleVar v        -> mkt ("SimpleVar " ^ name v) []
  | FieldVar (v,f)     -> mkt "FieldVar" [tree_of_lvar v; mkt (name f) []]
  | SubscriptVar (v,i) -> mkt "SubscriptVar" [tree_of_lvar v; tree_of_lexp i]

and tree_of_dec = function
  | VarDec (v,t,e)        -> mkt "VarDec"
                               [ mkt (name v) []
                               ; mkt (match t with None -> "" | Some (_,k) -> name k) []
                               ; tree_of_lexp e
                               ]
  | MutualFunctionDecs fs -> mkt "MutualFunDecs" (map tree_of_lfdec fs)
  | MutualTypeDecs ts     -> mkt "MutualTypDecs"
                               (map
                               (fun (_,(t,ty)) ->
                                 mkt "TypeDec" [mkt (name t) []; tree_of_lty ty])
                               ts)

and tree_of_lfdec (_,(f,ps,r,b)) =
  mkt ("Function: " ^ name f)
    [ mkt "Params" (map
                      (fun (_,(n,t)) ->
                        mkt (name n) [ mkt (name t) [] ])
                      ps)
    ; mkt (match r with None -> "FunctionDec" | Some (_,k) -> name k) []
    ; tree_of_lexp b
    ]

and tree_of_ty = function
  | NameTy t      -> mkt "NameTy" [mkt (name t) []]
  | RecordTy fs   -> mkt "RecordTy" (map
                                      (fun (_,(n,t)) ->
                                        mkt "" [mkt (name n) []; mkt (name t) []])
                                      fs)
  | ArrayTy (_,t) -> mkt "ArrayTy" [mkt (name t) []]

and tree_of_lexp (_,x) = tree_of_exp x
and tree_of_lvar (_,x) = tree_of_var x
and tree_of_ldec (_,x) = tree_of_dec x
and tree_of_lty  (_,x) = tree_of_ty  x
