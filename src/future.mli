type 'a future

(* future f x executes f x in a separate thread *)
val future : ('a -> 'b) -> 'a -> 'b future

(* block and return the result of the computation when it completes *)
val force : 'a future -> 'a
