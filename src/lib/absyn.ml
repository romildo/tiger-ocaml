(* absyn.ml *)

open Location

type symbol = Symbol.symbol
[@@deriving show]

type oper =
  | PlusOp | MinusOp | TimesOp | DivideOp
  | LtOp | LeOp | GtOp | GeOp
  | EqOp | NeOp
  | OrOp | AndOp
[@@deriving show]

type field = symbol * symbol
[@@deriving show]

and exp =
  | NilExp
  | IntExp    of int
  | StringExp of string
  | ArrayExp  of symbol * lexp * lexp
  | RecordExp of symbol * (symbol * lexp) loc list
  | VarExp    of lvar
  | AssignExp of lvar * lexp
  | CallExp   of symbol * lexp list
  | OpExp     of oper * lexp * lexp
  | IfExp     of lexp * lexp * lexp option
  | WhileExp  of lexp * lexp
  | ForExp    of symbol * lexp * lexp * lexp
  | BreakExp
  | SeqExp    of lexp list
  | LetExp    of ldec list * lexp
[@@deriving show]

and var =
  | SimpleVar    of symbol
  | FieldVar     of lvar * symbol
  | SubscriptVar of lvar * lexp
[@@deriving show]

and dec =
  | VarDec             of symbol * symbol loc option * lexp
  | MutualFunctionDecs of fundec loc list
  | MutualTypeDecs     of (symbol * lty) loc list
[@@deriving show]

and ty =
  | NameTy   of symbol
  | RecordTy of field loc list
  | ArrayTy  of symbol loc
[@@deriving show]

and fundec = symbol * field loc list * symbol loc option * lexp
[@@deriving show]

and lexp = exp loc
(* [@@deriving show] *)

and lvar = var loc
(* [@@deriving show] *)

and ldec = dec loc
(* [@@deriving show] *)

and lfundec = fundec loc
(* [@@deriving show] *)

and lty  = ty  loc
(* [@@deriving show] *)
