(* semantic.ml *)

open Error

module A = Absyn
module S = Symbol
module E = Env
module Ty = Types

(* let rec check_exp ... = ... *)

let type_check =
  check_exp (E.base_tenv, E.base_venv, false)
