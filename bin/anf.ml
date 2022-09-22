open Util

type atom =
  | Int of int
  | Var of var
  | Glob of var

and expr =
  | Return of atom
  (* return atom *)
  | Fun of var * var list * expr * body
  (* let var(var, list) = expr in body *)
  | Join of var * var option * expr * body
  (* join var(var) = expr in body *)
  | Jump of var * atom option
  (* goto var(atom) *)
  | App of var * var * atom list * body
  (* let var = var(atom, list) in body *)
  | Bop of var * bop * atom * atom * body
  (* let var = atom bop atom in body*)
  | If of atom * expr * expr
  (* if atom then expr else expr *)
  | Tuple of var * atom list * body
  (* let var = (atom; list) in body *)
  | Get of var * var * int * body
  (* let var = var[int] in body *)

and body = expr
and var = string
and bop = Lang.bop

let show_bop = Lang.show_bop

let print =
  let pr i = print_string (String.make i '\t'); print_endline in
  let f = Printf.sprintf in

  let show_atom = function
    | Int i -> string_of_int i
    | Var v -> v
    | Glob v -> "@" ^ v
  in

  let rec pe i = function
    | Return a -> pr i ("return " ^ show_atom a)

    | Fun (n, vs, e, body) ->
      let vs' = join_mapped id vs in
      pr i (f "let %s(%s) =" n vs');
      pe (i + 1) e;
      pr i "in"; pe i body;
      
    | Join (l, v, e, body) ->
      let v' = match v with Some v -> v | None -> "" in
      pr i (f "join %s(%s) =" l v');
      pe (i + 1) e;
      pr i "in"; pe i body;

    | Jump (l, v) ->
      let v' = match v with Some a -> "(" ^ show_atom a ^ ")" | _ -> "" in
      pr i (f "goto %s%s" l v')

    | App (d, c, vs, body) ->
      let vs' = join_mapped show_atom vs in
      pr i (f "let %s = %s(%s)" d c vs');
      pr i "in"; pe i body

    | Bop (d, op, a1, a2, body) ->
      let (op', a1', a2') = show_bop op, show_atom a1, show_atom a2 in
      pr i (f "let %s = %s %s %s"d a1' op' a2');
      pr i "in"; pe i body;

    | If (c, t, e) ->
      pr i ("if " ^ show_atom c ^ " then");
      pe (i + 1) t;
      pr i ("else");
      pe (i + 1) e;

    | Tuple (d, es, body) ->
      let es = join_mapped show_atom es in
      pr i (f "let %s = (%s)" d es);
      pr i "in"; pe i body

    | Get (d, v, n, body) ->
      pr i (f "let %s = %s[%d]" d v n);
      pr i "in"; pe i body

  in pe