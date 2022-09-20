let fresh =
  let c = ref (-1) in
  fun p ->
    incr c;
    p ^ string_of_int !c