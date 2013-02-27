open Insertionsort
open Selectionsort
open Mergesort
open Quicksort
open Bubblesort
open Heapsort

type 'a bench_entry = {
    name : string;
    fn : ('a array -> unit);
    mutable time : float
  }

let time f =
  let t1 = Unix.gettimeofday () in
  let r = f () in
  let t2 = Unix.gettimeofday () in
  (r, t2 -. t1)


let run_benchmark iters max_elems =
  let bench_entries = [|
    {name="insertion sort"; fn=insert_sort_arr; time=0.0};
    {name="bubble sort"; fn=bubblesort; time=0.0};
    {name="selection sort"; fn=selection_sort_arr; time=0.0};
    {name="merge sort"; fn=mergesort_arr; time=0.0};
    {name="merge sort + selection sort"; fn=mergesort_arr_mod selection_sort_arr; time=0.0};
    {name="merge sort + insertion sort"; fn=mergesort_arr_mod insert_sort_arr; time=0.0};
    {name="quicksort 1"; fn=quicksort; time=0.0};
    {name="quicksort 2"; fn=quicksort2; time=0.0};
    {name="heapsort"; fn=heapsort; time=0.0};
    {name="Array.sort"; fn=Array.sort compare; time=0.0};
    {name="Array.stable_sort"; fn=Array.stable_sort compare; time=0.0};
  |] in
  let num_entries = Array.length bench_entries in

  for i = 1 to iters do
    let num_elems = Random.int max_elems in
    let arr = Utils.make_random_int_array num_elems in
    let arrs = Array.init num_entries (fun _ -> Array.copy arr) in

    Array.iteri (fun j b ->
      let (_, t) = time (fun () -> b.fn arrs.(j)) in
      b.time <- b.time +. t
    ) bench_entries;

    Printf.printf "\r%6.2f%%" (float_of_int i /. float_of_int iters *. 100.0);
    flush_all ();
  done;

  Array.stable_sort (fun a b -> compare a.time b.time) bench_entries;
  Printf.printf "\r%-36s: %10s \t %10s\n" "Algorithm" "Total" "Average";
  Printf.printf "%s\n" (String.make 67 '=');
  Array.iteri (fun i b ->
    Printf.printf "%2d. %-32s: %10.6f \t %10.6f\n" (i+1) b.name b.time (b.time /. float_of_int iters)
  ) bench_entries

let _ =
  let iters = ref 100 in
  let max_elems = ref 5000 in

  Arg.parse [
    ("--iters", Arg.Set_int iters, "number of iterations. default: 100");
    ("--max-elems", Arg.Set_int max_elems, "maximum number of elements in an array. default: 5000");
  ] ignore "Usage";

  run_benchmark !iters !max_elems
