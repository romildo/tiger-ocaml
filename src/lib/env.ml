(* env.ml *)

module Ty = Types

let standard_types =
  [ ("int",    Ty.INT   )
  ; ("string", Ty.STRING)
  ]

let standard_functions =
  [ "printint",  [Ty.INT                   ], Ty.UNIT
  ; "exit",      [Ty.INT                   ], Ty.UNIT
  (* TODO: complete with the other standard functions *)
  ]

let base_tenv =
  List.fold_left
    (fun env (name, t) -> Symbol.enter (Symbol.symbol name) t env)
    Symbol.empty
    standard_types

let base_venv =
  List.fold_left
    (fun env (name, formals, result) ->
      Symbol.enter
        (Symbol.symbol name)
        (Ty.FUNCTION (formals, result))
        env)
    Symbol.empty
    standard_functions
