open Graphics
open VoroGeo
open VoroSeeds

module type ElementType =
  sig
    type elt
    val k: int
    val axis_get: int -> elt -> int
  end

module type Kd =
  sig
    type 'a kdtree
    type elt
    val build : elt list -> elt kdtree
    val search_near_point : point -> seed kdtree -> color option
    val print_tree : seed kdtree -> unit
    val draw_tree : seed kdtree -> unit
  end

module Make(Elt: ElementType): Kd with type elt = Elt.elt
