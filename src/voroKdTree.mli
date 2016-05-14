open Graphics
open VoroGeo
open VoroSeeds

module type ElementType =
  sig
    type elt
    type dem
    val k: int
    val axis_get: elt -> int -> dem
    val compare: dem -> dem -> int
  end

module type Kd =
  sig
    type kdtree
    val build : seed list -> kdtree
    val search_near_point : point -> kdtree -> color option
    val print_tree : kdtree -> unit
    val draw_tree : kdtree -> unit
  end

module Make(Elt: ElementType): Kd
