module type FutureConfig =
  sig
    val process_limit : int
  end

module type F =
  sig
    type 'a future

    (* future f x executes f x in a separate thread *)
    val future : ('a -> 'b) -> 'a -> 'b future

    (* block and return the result of the computation when it completes *)
    val force : 'a future -> 'a
  end

module Make (Config: FutureConfig): F

module Default: F
