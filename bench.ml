open Insertionsort
open Selectionsort
open Mergesort
open Quicksort
open Bubblesort

let time f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (r, t2 -. t1)

let make_random_int_array num_elems =
  let arr = Array.make num_elems 0 in
  Array.iteri (fun i _ -> arr.(i) <- (Random.int 1000)-500) arr;
  arr

let make_random_float_array num_elems =
  let arr = Array.make num_elems 0.0 in
  Array.iteri (fun i _ -> arr.(i) <- (Random.float 1000.0) -. 500.0) arr;
  arr

let _ =
  let iters = 100 in
  let funcs = [|
                 ("insertion sort", insert_sort_arr);
                 ("bubble sort", bubblesort);
                 ("selection sort", selection_sort_arr);
                 ("merge sort", mergesort_arr);
                 ("merge sort + selection sort", mergesort_arr_mod selection_sort_arr);
                 ("merge sort + insertion sort", mergesort_arr_mod insert_sort_arr);
                 ("quicksort 1", quicksort);
                 ("quicksort 2", quicksort2);
              |] in
  let total_times = Array.make (Array.length funcs) 0.0 in
  for i = 1 to iters do
    let num_elems = Random.int 10000 in
    let arr = make_random_float_array num_elems in
    let arrs = Array.make (Array.length funcs) arr in

    Array.iteri (fun i (_, f) ->
      let (_, t) = time (fun () -> f arrs.(i)) in
      total_times.(i) <- total_times.(i) +. t
    ) funcs;

    Printf.printf "\r%6.2f%%" (float_of_int i /. float_of_int iters *. 100.0);
    flush_all ();
  done;

  print_newline ();
  Printf.printf "%-32s: Total\t\tAverage\n" "Algorithm";
  Printf.printf "%s\n" (String.make 60 '=');
  Array.iteri (fun i (name, _) ->
    Printf.printf "%-32s: %f\t%f\n" name total_times.(i) (total_times.(i) /. float_of_int iters)
  ) funcs;
