let make_random_int_array num_elems =
  let arr = Array.make num_elems 0 in
  Array.iteri (fun i _ -> arr.(i) <- (Random.int 1000)-500) arr;
  arr

let make_random_float_array num_elems =
  let arr = Array.make num_elems 0.0 in
  Array.iteri (fun i _ -> arr.(i) <- (Random.float 1000.0) -. 500.0) arr;
  arr
