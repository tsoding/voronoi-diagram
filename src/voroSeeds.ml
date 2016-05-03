open VoroGeo
open Graphics

type seed = point * color

let generate_random_color (): color =
  (rgb (Random.int 255)
       (Random.int 255)
       (Random.int 255))

let generate_seed (rect: rect): seed =
  (generate_random_point rect,
   generate_random_color ())
