open Graphics
open VoroGeo
open VoroSeeds

module type ElementType =
  sig
    type elt
    val k: int
    val axis_get: int -> elt -> int
    val as_string: elt -> string
    val draw: rect -> unit
  end

module type Kd =
  sig
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil
    type elt
    val build : elt list -> elt kdnode
    val search_near_point : point -> seed kdnode -> color option
    val print_tree : elt kdnode -> unit
    val draw_tree : seed kdnode -> unit
  end

module Make(Elt: ElementType): Kd with type elt = Elt.elt
