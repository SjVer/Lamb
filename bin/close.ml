open Anf
open Fresh

module VS = Set.Make(String)

let used_vars e =
  let open VS in
  let in_atom = function Var v -> singleton v | _ -> empty in

  let rec go acc = function
    | Return a -> union (in_atom a) acc
    | Fun (f, _, e, b) ->
      let in_value = go acc e in
      let in_body = remove f (go acc b) in
      union in_value in_body

    | _ -> acc
  
in go empty e

let convert =
  let go = function
    | Fun (f, args, b, _) ->
      let _ = fresh "env" in
      let free = VS.(elements (diff (used_vars b) (of_list args))) in
      Printf.printf "free in %s:\n" f;
      List.iter (fun s -> Printf.printf "\t%s\n" s) free
    
    | _ -> ()
  in go