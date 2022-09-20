(*
  Alpha conversion

  the process of making each variable's name unique
  so that e.g.
    \x . \x . x + x
  becomes
    \x0 . \x1 . x1 + x1
*)

open Lang
module SMap = Map.Make(String)

let convert =
  let rec f env = function
    | Var x -> begin
      (* if this variable has already been renamed
          we just use that. otherwise its an error
          because its a 'use of unbound var x'. *)
      match SMap.find_opt x env with
        | Some x' -> Var x'
        | _ -> failwith "Scope checking failed!"
      end
    | Lam (x, e) ->
      (* introduce the lambda's parameter as a fresh
         variable and include it in the body's env. *)
      let x' = Fresh.fresh x in
      Lam (x', f (SMap.add x x' env) e)

    (* the rest of this is just mapping to converted *)
    | Int _ as t -> t
    | App (e1, e2) -> App (f env e1, f env e2)
    | Bop (op, e1, e2) -> Bop (op, f env e1, f env e2)
    | If (c, t, e) -> If (f env c, f env t, f env e)
  in f SMap.empty