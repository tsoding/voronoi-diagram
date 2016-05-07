open Graphics
open VoroGeo
open VoroSeeds

type kdtree

val build : seed list -> kdtree

val search_near_point : point -> kdtree -> color option

val print_tree : kdtree -> unit
