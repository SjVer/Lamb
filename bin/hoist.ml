open Anf
open Fresh

(** let var(var, list) =
      join: ...
      join: ...
      list: ... *)
type func = var * var list * expr

type 'a dlist = 'a list -> 'a list
let cons x = fun xs -> x :: xs
let nil = fun xs -> [] @ xs
let (@-) xs ys tl = xs (ys tl)

let hoist e : func list =
  (* collect al functions and the
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

  let rec go : expr -> func dlist * expr = function
    | Return _  | Break _ | App _ | Bop _ | Tuple _ | Get _
      as e -> (nil, e)
    
    | Fun (f, xs, v, b) ->
      (* gather functions in value and body *)
      let (v_fs, v') = go v in
      let (b_fs, b') = go b in
      (* create an entry join containing the func's body *)
      let fn = (f, xs, v') in
      (* return all functions and joins *)
      (v_fs @- b_fs @- cons fn, b')  

    | If (d, c, t, e, b) ->
      let (t_fs, t') = go t in
      let (e_fs, e') = go e in
      let (b_fs, b') = go b in
      let expr = If (d, c, t', e', b') in
      (e_fs @- t_fs @- b_fs, expr)      
  in
  let (funcs, expr) = go e in
  (* pack everything up and return *)
  let main : func = ("main", [], expr) in
  funcs [] @ [main]