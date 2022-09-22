open Lang

let () =
  (* (\x . \x . x + x) 2 *)
  let e = App (Lam ("x", Lam ("y", Bop (Add, Var "x", Var "y"))), Int 2) in

  print_endline "expression:";
  print e;
  
  let e = Alpha.convert e in
  print_endline "\nalpha-conversion:";
  print e;
  
  let e = Norm.normalize e in
  print_endline "\na-normalization:";
  Anf.print e;
  

  let e = Close.convert e in
  print_endline "\nclosure-conversion:";
  Anf.print e;