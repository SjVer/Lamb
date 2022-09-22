type expr =
  | Int of int
  | Var of var
  | Lam of var * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr

and var = string
and bop = Add | Sub | Mul | Div

let show_bop = function
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let print e =
  let pr = print_string in
  let f = Printf.sprintf in
  let opn simple = (if simple then pr "(") in
  let cls simple = (if simple then pr ")") in

  let rec pe simple = function 
    | Int l -> pr (string_of_int l)
    | Var v -> pr v
    | Lam (v, b) ->
      opn simple;
      pr (f "\\%s . " v);
      pe false b;
      cls simple
    | App (e1, e2) ->
      pe true e1; pr " "; pe false e2
    | Bop (op, e1, e2) ->
      pe true e1;
      pr (f " %s " (show_bop op));
      pe false e2
    | If (c, t, e) ->
      pr "if "; pe false c;
      pr " then "; pe false t;
      pr " else "; pe false e
  in
  print_char '\t';
  pe false e;
  print_newline ()