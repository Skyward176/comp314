val print_int_list : int list -> unit
val print_string_list : string list -> unit
val print_int_list_list : int list list -> unit
val print_string_list_list : string list list -> unit
val cond_dup : 'a list -> ('a -> bool) -> 'a list
val n_times : ('a -> 'a) * int * 'a -> 'a
val zipwith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val findEquiv :
  ('a list -> 'a list -> bool) -> 'a list -> 'a list list -> 'a list
val recBuckets : ('a list -> 'a list -> bool) -> 'a list list -> 'a list
val buckets : (int list -> int list -> bool) -> int list list -> 'a list
val fib_tailrec : 'a -> int
val sum_rows : int list list -> int list
val ap : 'a -> 'b -> 'c list
val prefixes : 'a -> 'b list
val powerset : 'a -> 'b list
val assoc_list : 'a -> 'b list
val main : unit -> unit
