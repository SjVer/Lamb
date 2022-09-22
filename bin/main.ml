open Lang

let print_fns =
  let print_jn (n, p, e) =
    let p' = Option.value ~default:"" p in
    Printf.printf "\t\tjoin %s(%s):\n" n p';
    Anf.print 3 e
  in

  let go (f, xs, e, jns) =
    Printf.printf "\tlet %s(%s) =\n" f Util.(join_mapped id xs);
    print_jn e;
    List.iter (fun x -> print_newline (); print_jn x) jns

  in function
    | first :: rest ->
      go first;
      List.iter (fun x -> print_newline (); go x) rest
    | [] -> ()

let () =
  (* (\x . \x . x + x) 2 *)
  (* let e = Lam ("x", Lam ("y", Bop (Add, Var "x", Var "y"))) in *)
  let e = Lam ("x", Lam ("y", If (Var "x", Var "y", Int 0))) in

  print_endline "expression:";
  print e;
  
  let e = Alpha.convert e in
  print_endline "\nalpha-conversion:";
  print e;
  
  let e = Norm.normalize e in
  print_endline "\na-normalization:";
  Anf.print 1 e;
  

  let e = Close.convert e in
  print_endline "\nclosure-conversion:";
  Anf.print 1 e;

  let fs = Hoist.hoist e in
  print_endline "\nhoisting:";
  print_fns fs;

  print_endline "\nlowering:";
  Lower.lower fs;