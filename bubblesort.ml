let bubblesort xs =
  let swap i j =
    let t = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- t
  in

  let len = Array.length xs in
  for i = len - 1 downto 0 do
    for j = 0 to i-1 do
      if xs.(j) > xs.(j+1) then
        swap j (j+1)
    done
  done
