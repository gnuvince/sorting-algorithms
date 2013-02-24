let rec mergesort_arr xs =
  let merge dst left right =
    let i = ref 0 in
    let j = ref 0 in
    let k = ref 0 in

    while !j < Array.length left && !k < Array.length right do
      if left.(!j) < right.(!k) then begin
        dst.(!i) <- left.(!j);
        incr j
      end
      else begin
        dst.(!i) <- right.(!k);
        incr k
      end;
      incr i
    done;

    while !j < Array.length left do
      dst.(!i) <- left.(!j);
      incr j;
      incr i
    done;

    while !k < Array.length right do
      dst.(!i) <- right.(!k);
      incr k;
      incr i
    done
  in

  match xs with
  | [||] -> ()
  | [|_|] -> ()
  | _ -> let len = Array.length xs in
         let left = Array.make (len/2) xs.(0) in
         let right = Array.make (len - len/2) xs.(0) in
         Array.blit xs 0 left 0 (len/2);
         Array.blit xs (len/2) right 0 (len-len/2);
         mergesort_arr left;
         mergesort_arr right;
         merge xs left right





let rec mergesort_arr_mod altsort xs =
  let merge dst left right =
    let i = ref 0 in
    let j = ref 0 in
    let k = ref 0 in

    while !j < Array.length left && !k < Array.length right do
      if left.(!j) < right.(!k) then begin
        dst.(!i) <- left.(!j);
        incr j
      end
      else begin
        dst.(!i) <- right.(!k);
        incr k
      end;
      incr i
    done;

    while !j < Array.length left do
      dst.(!i) <- left.(!j);
      incr j;
      incr i
    done;

    while !k < Array.length right do
      dst.(!i) <- right.(!k);
      incr k;
      incr i
    done
  in

  if Array.length xs < 7 then begin
    altsort xs
  end
  else begin
    let len = Array.length xs in
    let left = Array.make (len/2) xs.(0) in
    let right = Array.make (len - len/2) xs.(0) in
    Array.blit xs 0 left 0 (len/2);
    Array.blit xs (len/2) right 0 (len-len/2);
    mergesort_arr_mod altsort left;
    mergesort_arr_mod altsort right;
    merge xs left right
  end
