open Insertionsort
open Selectionsort
open Mergesort
open Quicksort
open Bubblesort
open Heapsort

let is_sorted xs =
  let limit = Array.length xs - 2 in
  let rec loop i =
    if i > limit then
      true
    else if xs.(i) > xs.(i+1) then
      false
    else
      loop (i+1)
  in
  loop 0


let test n sort =
  let ok = ref true in
  for i = 1 to n do
    let arr = Utils.make_random_int_array 64 in
    sort arr;
    if not (is_sorted arr) then
      ok := false
  done;
  !ok

let _ =
  let n = 100 in
  let funcs = [
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
  ] in
  List.iter (fun (name, sort) ->
    Printf.printf "%-32s: %s\n" name (if test n sort then "OK" else "FAIL");
    flush_all ()
  ) funcs
