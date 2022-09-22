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
let cons x = fun xs -> x :: xs
let nil = fun xs -> [] @ xs
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
    | Return _ | Jump _ | App _ | Bop _ | Tuple _ | Get _
      as e -> (nil, nil, e)
    
    | Join (j, p, v, b) ->
      (* gather functions and joins in value and body *)
      let (v_fs, v_js, v') = go v in
      let (b_fs, b_js, b') = go b in
      (* create a new join and append and return it *)
      let jn = (j, p, v') in
      (v_fs @- b_fs, v_js @- b_js @- cons jn, b')

    | Fun (f, xs, v, b) ->
      (* gather functions and joins in value and body *)
      let (v_fs, v_js, v') = go v in
      let (b_fs, b_js, b') = go b in
      (* create an entry join containing the func's body *)
      let entry = fresh "entry" in
      let fn = (f, xs, (entry, None, v'), v_js []) in
      (* return all functions and joins *)
      (v_fs @- b_fs @- cons fn, b_js, b')  

    | If (c, t, e) ->
      let (e_fs, e_js, e') = go e in
      let (t_fs, t_js, t') = go t in
      let thn, els = fresh "then", fresh "else" in
      let thn_join, els_join = (thn, None, t'), (els, None, e') in
      let if_expr = If (c, Jump (thn, None), Jump (els, None)) in
      (t_fs @- e_fs, t_js @- e_js @- cons thn_join @- cons els_join, if_expr)
  in
  let (funcs, joins, expr) = go e in
  (* pack everything up and return *)
  let entry = (fresh "entry", None, expr) in
  let main : func = ("main", [], entry, joins []) in
  funcs [] @ [main]