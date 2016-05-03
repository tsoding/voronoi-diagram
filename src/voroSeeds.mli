open Graphics
open VoroGeo

type seed = point * color

val generate_random_color: unit -> color

val generate_seed: rect -> seed

val generate_seeds: rect -> int -> seed list

val get_color: seed list -> point -> distance_function -> color
