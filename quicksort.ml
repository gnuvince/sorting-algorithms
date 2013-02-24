let quicksort xs =
  let swap i j =
    let t = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- t
  in

  let partition left right pivot =
    let pivotValue = xs.(pivot) in
    swap pivot right;
    let storeIndex = ref left in
    for i = left to right - 1 do
      if xs.(i) < pivotValue then begin
        swap i !storeIndex;
        incr storeIndex
      end
    done;
    swap !storeIndex right;
    !storeIndex
  in

  let rec qsort left right =
    if left < right then begin
      let pivot = left + Random.int (right - left) in
      let newPivot = partition left right pivot in
      qsort left (newPivot-1);
      qsort (newPivot+1) right
    end
  in

  qsort 0 (Array.length xs - 1)


let quicksort2 xs =
  let swap i j =
    let t = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- t
  in

  let split start length pivot_pos =
    let pivot = xs.(pivot_pos) in
    swap start pivot_pos;

    let lo, hi = ref (start+1), ref (start+length-1) in
    while !lo < !hi do
      while !lo < !hi && xs.(!lo) <= pivot do incr lo done;
      while !lo < !hi && xs.(!hi) >  pivot do decr hi done;
      if !lo < !hi then
        swap !lo !hi
    done;
    if xs.(!lo) > pivot then decr lo;
    swap start !lo;
    !lo
  in

  let rec qsort start length =
    if length > 1 then begin
      let pivot_pos = start + Random.int length in
      let new_pos = split start length pivot_pos in
      qsort start (new_pos - start);
      qsort (new_pos+1) (start + length - new_pos - 1)
    end
  in
  qsort 0 (Array.length xs)
