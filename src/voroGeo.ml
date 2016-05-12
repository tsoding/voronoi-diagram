type point = int * int
type rect = {
    position: point;
    size: int * int;
  }
type distance_function = point -> point -> float

let make_rect (x: int) (y: int) (w: int) (h: int): rect =
  { position = (x, y);
    size = (w, h) }

let pnorm_distance (p: int)
                   (x1, y1: point)
                   (x2, y2: point): float =
  let dx = float_of_int @@ abs (x1 - x2) in
  let dy = float_of_int @@ abs (y1 - y2) in
  let fp = float_of_int p in
  let inverted_fp = 1.0 /. fp in
  (dx ** fp +. dy ** fp) ** inverted_fp

let euclidean_distance: distance_function = pnorm_distance 2

let taxicab_distance: distance_function = pnorm_distance 1

let generate_random_point {position; size}: point =
  let (x, y) = position in
  let (w, h) = size in
  (x + Random.int w, y + Random.int h)
