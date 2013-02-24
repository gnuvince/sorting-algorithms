let rec insert_sort_list xs =
  let rec insert x ys =
    match ys with
    | [] -> [x]
    | y::ys' -> if x < y then x::ys else y :: insert x ys'
  in

  match xs with
  | [] -> []
  | [x] -> [x]
  | x::xs' -> insert x (insert_sort_list xs')


let insert_sort_arr xs =
  for j = 1 to Array.length xs - 1 do
    let elem = xs.(j) in
    let i = ref (j-1) in
    while !i >= 0 && xs.(!i) > elem do
      xs.(!i+1) <- xs.(!i);
      decr i
    done;
    xs.(!i+1) <- elem
  done
