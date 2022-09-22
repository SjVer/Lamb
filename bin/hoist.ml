open Anf

type join = var * var option * expr
type func = var * var list * join * join list
type 'a dlist = 'a list -> 'a list
let cons x xs = x :: xs
let (@-) xs ys tl = xs (ys tl)

let hoist =
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

  let rec go =