let bubblesort xs =
  let swap i j =
    let t = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- t
  in

  for i = 0 to Array.length xs - 2 do
    for j = 0 to Array.length xs - 2 - i do
      if xs.(j) > xs.(j+1) then
        swap j (j+1)
    done
  done
