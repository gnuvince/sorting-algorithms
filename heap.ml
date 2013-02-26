type 'a t = {
  mutable size : int;
  mutable elems : 'a array
}

let make_heap init_size sample_element = {size=0;
                                          elems=Array.make init_size sample_element}

let parent i = (i-1)/2

let heap_add heap elem =
  if Array.length heap.elems = heap.size then begin
    let new_elems = Array.make (2*heap.size) heap.elems.(0) in
    Array.blit heap.elems 0 new_elems 0 heap.size;
    heap.elems <- new_elems
  end;
  heap.elems.(heap.size) <- elem;
  heap.size <- heap.size + 1

let swap xs i j =
  let t = xs.(i) in
  xs.(i) <- xs.(j);
  xs.(j) <- t

let insert heap elem =
  heap_add heap elem;
  let i = ref (heap.size-1) in
  while !i > 0 && heap.elems.(!i) < heap.elems.(parent !i) do
    swap heap.elems !i (parent !i);
    i := parent !i
  done

let delete_min heap =
  let min_child i =
    if heap.size > 2*i+2 then
      if heap.elems.(2*i+1) <= heap.elems.(2*i+2) then
        Some (2*i+1)
      else
        Some (2*i+2)
    else if heap.size > 2*i+1 then
      Some (2*i+1)
    else
      None
  in

  let min_elem = heap.elems.(0) in
  heap.size <- heap.size - 1;
  heap.elems.(0) <- heap.elems.(heap.size);

  let is_done = ref false in
  let i = ref 0 in
  while not (!is_done) do
    match min_child !i with
    | None -> is_done := true
    | Some j ->
      if heap.elems.(!i) <= heap.elems.(j) then
        is_done := true
      else begin
        swap heap.elems !i j;
        i := j
      end
  done;
  min_elem



let test () =
  let h = make_heap 1 0 in
  insert h 3;
  insert h 5;
  insert h 1;
  h
