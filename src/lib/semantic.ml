(* semantic.ml *)

module A = Absyn
module S = Symbol
module E = Env
module T = Types


let type_mismatch loc expected found =
  Error.error loc "type mismatch: expected %s, found %s" (T.string_of_ty expected) (T.string_of_ty found)

let undefined loc kind id =
  Error.error loc "undefined %s %s" kind (S.name id)

let misdefined loc kind id =
  Error.error loc "%s is not a %s" (S.name id) kind

let cannot_be_nil loc id =
  Error.error loc "cannot initialize untyped variable %s with nil" (S.name id)

let loc = Location.loc

let coerceable = T.coerceable

let look env kind id pos =
  match S.look id env with
  | Some x -> x
  | None -> undefined pos kind id

let tylook tenv id pos =
  look tenv "type" id pos

let funlook venv id pos =
  look venv "function" id pos

let varlook venv id pos =
  look venv "variable" id pos

let coerce ty1 ty2 pos =
  if not (coerceable ty1 ty2) then
    type_mismatch pos ty2 ty1

let check_int ty pos = coerce ty T.INT pos

let check_unit ty pos = coerce ty T.UNIT pos

let rec check_exp ((tenv,venv,in_loop) as env) (pos,exp) =
  match exp with
  | A.NilExp -> T.NIL

  | A.IntExp _ -> T.INT

  | A.ArrayExp (typeid, ((lsize,_) as size), elem) ->
     let tsize = check_exp env size in
     check_int tsize lsize;
     let telem = check_exp env elem in
     begin match tylook tenv typeid pos with
     | T.ARRAY (te,_) as t ->
        coerce telem te (loc elem);
        t
     | _ -> misdefined pos "array type" typeid
     end

  | A.OpExp (op,l,r) ->
    let tl = check_exp env l in
    let tr = check_exp env r in
    begin match op with
    | A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp ->
       check_int tl (loc l);
       check_int tr (loc r);
       T.INT
    (* TODO: remaining binary operators *)
    | _ ->
       Error.fatal "unimplemented"
    end

  | A.VarExp var -> check_var env var

  | A.SeqExp exps ->
     let rec check_seq = function
       | []        -> T.UNIT
       | [exp]     -> check_exp env exp
       | exp::rest -> ignore (check_exp env exp); check_seq rest
     in
     check_seq exps

  | A.LetExp (decs,body) ->
     let env' = List.fold_left check_dec env decs in
     check_exp env' body

  (* TODO: remaining expression *)

  | _ ->
     Error.fatal "unimplemented"

and check_var ((tenv,venv,in_loop) as env) (pos,var) =
  match var with
  | A.SimpleVar id ->
     begin match varlook venv id pos with
     | T.FUNCTION _ -> misdefined pos "variable" id
     | t -> t
     end

  (* TODO: remaining variables  *)

  | _ ->
     Error.fatal "unimplemented"

and check_dec ((tenv,venv,in_loop) as env) (pos,dec) =
  match dec with
  | A.VarDec (name,type_opt,init) ->
     let tinit = check_exp env init in
     let tvar =
       match type_opt with
       | Some (pos,tname) -> let t = tylook tenv tname pos in
                             coerce tinit t (loc init);
                             t
       | None -> if coerceable tinit T.NIL then
                   cannot_be_nil (loc init) name;
                 tinit
     in
     let venv' = S.enter name tvar venv in
     (tenv,venv',in_loop)

  | A.MutualTypeDecs tdecs ->
     (* first pass: add new type names to environment *)
     let new_tenv =
       List.fold_left
         (fun tenv (_pos, (tname, _tcons)) ->
           S.enter tname (T.NAME (tname, ref None)) tenv)
         tenv
         tdecs
     in
     let new_env = (new_tenv, venv, in_loop) in
     (* second pass: check type definition *)
     List.iter
       (fun (_pos, (tname, tcons)) ->
         let ty = check_ty new_env tcons in
         match S.look tname new_tenv with
         | Some (T.NAME (_, cell)) -> cell := Some ty
         | _ -> Error.fatal "bug!")
       tdecs;
     new_env

  (* TODO: remaining declarations  *)

  | _ ->
     Error.fatal "unimplemented"

and check_ty ((tenv,venv,in_loop) as env) (pos,ty) =
  match ty with
  | A.NameTy t -> tylook tenv t pos

  (* TODO: remaining type constructors *)

  | _ ->
     Error.fatal "unimplemented"


let type_check program =
  check_exp (E.base_tenv, E.base_venv, false) program
