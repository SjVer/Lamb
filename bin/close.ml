open Anf
open Fresh

module VS = Set.Make(String)

let free_vars e =
  let open VS in
  let in_atom = function Var v -> singleton v | _ -> empty in
  let ( @ ) = union in

  let rec go acc = function
    | Return a -> acc @ (in_atom a)

    | Fun (f, vs, e, b) ->
      let in_value = diff (go acc e) (of_list vs) in
      let in_body = remove f (go acc b) in
      acc @ in_value @ in_body

    | Bop (d, _, e1, e2, b) ->
      let in_value = (in_atom e1) @ (in_atom e2) in
      let in_body = remove d (go acc b) in
      acc @ in_value @ in_body

    | _ -> acc
  
  in go empty e

let convert =
  let rec go = function
    | Return _ as r -> r
    | Join (f, v, e, b) -> Join (f, v, go e, go b)
    | Jump _ as j -> j
    | Bop (d, op, e1, e2, b) -> Bop (d, op, e1, e2, go b)
    | If (c, t, e) -> If (c, go t, go e)
    | Tuple (d, es, b) -> Tuple (d, es, go b)
    | Get (d, v, i, b) -> Get (d, v, i, go b)

    | Fun (f, args, v, b) ->
      (* let f(args) = v in e *)
      let env = fresh "env" in
      
      (* find all free values in v and remove
         the arguments from f bc they are bound *)
      let free = VS.(elements (diff (free_vars v) (of_list args))) in

      (* helper function to create "let x = env[i] in e" *)
      let get (e, i) x = (Get (x, env, i, e), i + 1) in

      (* greates the `get`s from `env` *)
      let v' = fst (List.fold_left get (go v, 1) free) in
      let e' =
        let vs = List.map (fun v -> Var v) free in
        (* create the actual closure `(func; args...)` *)
        Tuple (f, Glob f :: vs, go b)
      in
      (* return then new function with the new value and body *)
      Fun (f, env :: args, v', e')

    | App (d, f, vs, b) ->
      (* transform "let d = f(vs) in e"
         to "let ptr = f[0] in let d = ptr(f, vs) in e" *)
      let ptr = fresh f in
      Get (ptr, f, 0, App (d, ptr, Var f :: vs, go b))

  in go