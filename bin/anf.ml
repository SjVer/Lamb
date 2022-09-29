open Util

type atom =
  | Int of int
  | Var of var
  | Glob of var

and expr =
  | Return of atom
  (* return atom *)
  | Break of atom
  (* break atom *)
  | Fun of var * var list * expr * body
  (* let var(var, list) = expr in body *)
  | App of var * var * atom list * body
  (* let var = var(atom, list) in body *)
  | Bop of var * bop * atom * atom * body
  (* let var = atom bop atom in body*)
  | If of var * atom * expr * expr * body
  (* let var = if atom then expr else expr in body *)
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
    | Break a -> pr i ("break " ^ show_atom a)

    | Fun (n, vs, e, body) ->
      let vs' = join_mapped id vs in
      pr i (f "let %s(%s) =" n vs');
      pe (i + 1) e;
      pe_body i body;
      
    | App (d, c, vs, body) ->
      let vs' = join_mapped show_atom vs in
      pr i (f "let %s = %s(%s)" d c vs');
      pe_body i body

    | Bop (d, op, a1, a2, body) ->
      let (op', a1', a2') = show_bop op, show_atom a1, show_atom a2 in
      pr i (f "let %s = %s %s %s"d a1' op' a2');
      pe_body i body;

    | If (d, c, t, e, body) ->
      pr i (f "let %s = if %s then" d (show_atom c));
      pe (i + 1) t;
      pr i ("else");
      pe (i + 1) e;
      pe_body i body

    | Tuple (d, es, body) ->
      let es = join_mapped show_atom es in
      pr i (f "let %s = (%s)" d es);
      pe_body i body

    | Get (d, v, n, body) ->
      pr i (f "let %s = %s[%d]" d v n);
      pe_body i body
  
  and pe_body i = pr i "in"; pe i

  in pe