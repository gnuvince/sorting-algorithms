let selection_sort_arr arr =
  let swap i j =
    let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t
  in

  for i = 0 to Array.length arr - 2 do
    let min = ref i in
    for j = i+1 to Array.length arr - 1 do
      if arr.(j) < arr.(!min) then
        min := j
    done;
    swap i !min
  done
