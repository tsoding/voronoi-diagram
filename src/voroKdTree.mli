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
    type kdtree
    type elt
    val build : seed list -> kdtree
    val search_near_point : point -> kdtree -> color option
    val print_tree : kdtree -> unit
    val draw_tree : kdtree -> unit
  end

module Make(Elt: ElementType): Kd
