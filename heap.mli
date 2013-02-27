type 'a t
exception Empty

val make_heap : int -> 'a -> 'a t
val insert : 'a t -> 'a -> unit
val delete_min : 'a t -> 'a
val size : 'a t -> int
