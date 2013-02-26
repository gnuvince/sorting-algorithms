let heapsort xs =
  if Array.length xs < 2 then
    ()
  else
    let heap = Heap.make_heap (Array.length xs) xs.(0) in
    Array.iter (Heap.insert heap) xs;
    Array.iteri (fun i _ -> xs.(i) <- Heap.delete_min heap) xs
