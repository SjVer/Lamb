open Lang
open Anf
open Fresh

type le = Lang.expr
type ae = Anf.expr
type aa = Anf.atom

let normalize e =
  let mk_ret v = Return v in
  let mk_brk v = Break v in
  let ( let* ) = (@@) in

  (* [cont] is a function that uses the produced `Anf.expr`.
     for example: when normalizing a lambda we pass `mk_ret`
     so that the `Anf.expr` that the normalization of the 
     lambda's body produces is put in `Return`. *)
  let rec go (e : le) (cont : aa -> ae) : ae = match e with
    | Int i -> cont (Int i)
    | Var v -> cont (Var v)

    | Lam (v, e) ->
      (* we create a function with fresh name `f` and
         normalize its body (passing `mk_ret` of course)
         and produce "let f(v) = e' in <do smth with f>" *)
      let f = fresh "f" in
      let e' = go e mk_ret in
      Fun (f, [v], e', cont (Var f))
    
    | App (f, v) ->
      (* we check that `f` is a named value and apply it.
         The return value is stored in fresh variable `r`
         producing "let r = f(v) in <do smth with r>" *)
      let* f' = go f in
      let* v' = go v in
      begin match f' with
        | Var f' ->
          let r = fresh "funr" in
          App (r, f', [v'], cont (Var r))
        | _ -> failwith "Can only apply named values"
      end
    
    | Bop (op, e1, e2) ->
      let* e1' = go e1 in
      let* e2' = go e2 in
      let r = fresh "bopr" in
      Bop (r, op, e1', e2', cont (Var r))
    
    | If (c, t, e) ->
      let* c' = go c in
      let r = fresh "phi" in
      If (r, c', go t mk_brk, go e mk_brk, cont (Var r))

  in go e mk_ret