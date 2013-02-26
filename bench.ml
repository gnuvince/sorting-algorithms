open Insertionsort
open Selectionsort
open Mergesort
open Quicksort
open Bubblesort
open Heapsort

let time f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (r, t2 -. t1)


let run_benchmark iters max_elems =
  let funcs = [|
                 ("insertion sort", insert_sort_arr);
                 ("bubble sort", bubblesort);
                 ("selection sort", selection_sort_arr);
                 ("merge sort", mergesort_arr);
                 ("merge sort + selection sort", mergesort_arr_mod selection_sort_arr);
                 ("merge sort + insertion sort", mergesort_arr_mod insert_sort_arr);
                 ("quicksort 1", quicksort);
                 ("quicksort 2", quicksort2);
                 ("heapsort", heapsort);
                 ("Array.sort", Array.sort compare);
                 ("Array.stable_sort", Array.stable_sort compare);
              |] in
  let total_times = Array.make (Array.length funcs) 0.0 in
  for i = 1 to iters do
    let num_elems = Random.int max_elems in
    let arr = Utils.make_random_int_array num_elems in
    let arrs = Array.init (Array.length funcs) (fun _ -> Array.copy arr) in

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
  ) funcs

let _ =
  let iters = ref 100 in
  let max_elems = ref 5000 in

  Arg.parse [
    ("--iters", Arg.Set_int iters, "number of iterations. default: 100");
    ("--max-elems", Arg.Set_int max_elems, "maximum number of elements in an array. default: 5000");
  ] ignore "Usage";

  run_benchmark !iters !max_elems
