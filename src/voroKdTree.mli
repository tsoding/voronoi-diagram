open Graphics
open VoroGeo
open VoroSeeds

module type ElementType =
  sig
    type elt
    val k: int
    val make : int list -> elt
    val axis_get: int -> elt -> int
    val as_string: elt -> string
    val distance: elt -> elt -> float
  end

module type Kd =
  sig
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil
    type elt
    val build : elt list -> elt kdnode
    val search_near_point : distance_function -> point -> seed kdnode -> color option
    val search_near_point_general : elt -> elt kdnode-> elt option
    val print_tree : elt kdnode -> unit
  end

module Make(Elt: ElementType): Kd with type elt = Elt.elt
