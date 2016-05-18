module type ElementType =
  sig
    type elt
    val k: int
    val make : int list -> elt
    val axis_get: int -> elt -> int
    val as_string: elt -> string
  end

module type Kd =
  sig
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil
    type elt
    type elt_distance_function = elt -> elt -> float
    val build : elt list -> elt kdnode
    val search_near_point : elt_distance_function -> elt -> elt kdnode-> elt option
    val print_tree : elt kdnode -> unit
  end

module Make(Elt: ElementType): Kd with type elt = Elt.elt
