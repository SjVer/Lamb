open Lang

let () =
  (* \x . \x . x + x *)
  let e = Lam ("x", Lam ("y", Bop (Add, Var "x", Var "y"))) in

  print_endline "expression:";
  print e;
  
  let e = Alpha.convert e in
  print_endline "\nalpha-conversion:";
  print e;
  
  let e = Norm.normalize e in
  print_endline "\na-normalization:";
  Anf.print e;
  
  print_endline "\nclosure-conversion:";
  Close.convert e;