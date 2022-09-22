open Anf
open Fresh

(** join var(var) = expr *)
type join = var * var option * expr

(** let var(var, list) =
      join: ...
      join: ...
      list: ... *)
type func = var * var list * join * join list

type 'a dlist = 'a list -> 'a list
let cons x xs = x :: xs
let (@-) xs ys tl = xs (ys tl)

let hoist e : func list =
  (* collect al functions and joints and the
     final expression (the entry block). 
     for example:
        let f1 = ...
          let f2 = ...
          join j3 = ...
        in expr
     becomes:
        let f2 = ...
        let j3 = ...
        let f1 = expr *)

  let rec go : expr -> func dlist * join dlist * expr = function
    | Fun (f, xs, v, b) ->
      (* gather functions and joins in value and body *)
      let (v_funcs, v_joins, v') = go v in
      let (b_funcs, b_joins, b') = go b in
      (* create an entry join containing the func's body *)
      let entry = fresh "entry" in
      let fn = (f, xs, (entry, None, v'), v_joins []) in
      (* return all functions and joins *)
      (v_funcs @- b_funcs @- cons fn, b_joins, b')  
    
    | _ -> failwith ""
  in
  let (funcs, joins, expr) = go e in
  (* pack everything up and return *)
  let entry = (fresh "entry", None, expr) in
  let main : func = ("main", [], entry, joins []) in
  main :: (funcs [])