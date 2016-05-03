open VoroGeo
open Graphics

type seed = point * color

let min_with (f: 'a -> 'b) (a: 'a) (b: 'a): 'a =
  if (f a) < (f b) then a else b

let generate_random_color (): color =
  (rgb (Random.int 255)
       (Random.int 255)
       (Random.int 255))

let generate_seed (rect: rect): seed =
  (generate_random_point rect,
   generate_random_color ())

let generate_seeds (rect: rect) (n: int): seed list =
  BatList.Labels.init n (fun _ -> generate_seed rect)

let get_color (seeds: seed list)
              (p: point)
              (distance: distance_function): color =
  seeds
  |> List.map (fun ((x, y), color) -> (distance p (x, y), color))
  |> List.fold_left (min_with fst) (max_float, black)
  |> snd
