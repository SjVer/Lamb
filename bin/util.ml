let id x = x

let rec join_mapped f = function
  | [] -> ""
  | [x] -> f x
  | x::xs -> f x ^ ", " ^ join_mapped f xs