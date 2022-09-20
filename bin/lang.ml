type expr =
  | Int of int
  | Var of var
  | Lam of var * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr

and var = string

and bop = Add | Sub | Mul | Div

let print e =
  let pr = print_string in
  let f = Printf.sprintf in

  let show_bop = function
    Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" in

  let rec pe = function 
    | Int l -> pr (string_of_int l)
    | Var v -> pr v
    | Lam (v, b) ->
      pr (f "\\%s . " v);
      pe b
    | App (e1, e2) ->
      pe e1;
      pe e2
    | Bop (op, e1, e2) ->
      pe e1;
      pr (f " %s " (show_bop op));
      pe e2
    | If (c, t, e) ->
      pr "if "; pe c;
      pr " then "; pe t;
      pr " else "; pe e
  in pe e; print_newline ()