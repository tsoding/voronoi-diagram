open Graphics
open BatSeq

type point = int * int
type distance_function = point -> point -> float

let window_width = 800
let window_height = 600
let amount_of_point = 50

let min_with (f: 'a -> 'b) (a: 'a) (b: 'a): 'a =
  if (f a) < (f b) then a else b

let generate_point (): point * color =
  ((Random.int window_width, Random.int window_height),
   (rgb (Random.int 255) (Random.int 255) (Random.int 255)))

let generate_ps (n: int): (point * color) list =
  BatList.Labels.init n (fun _ -> generate_point ())

let ps: (point * color) list = generate_ps amount_of_point

let pnorm_distance (p: int) (x1, y1: point) (x2, y2: point): float =
  let dx = float_of_int @@ abs (x1 - x2) in
  let dy = float_of_int @@ abs (y1 - y2) in
  let fp = float_of_int p in
  let inverted_fp = 1.0 /. fp in
  (dx ** fp +. dy ** fp) ** inverted_fp

let euclidean_distance: distance_function = pnorm_distance 2

let taxicab_distance: distance_function = pnorm_distance 1

let get_color (p: point) (distance: distance_function): color =
  ps
  |> List.map (fun ((x, y), color) -> (distance p (x, y), color))
  |> List.fold_left (min_with fst) (max_float, black)
  |> snd

let draw_voronoi (distance: distance_function): unit =
  for y = 0 to window_height - 1 do
    for x = 0 to window_width - 1 do
      let color = get_color (x, y) distance in
      set_color color;
      plot x y
    done;
    if y mod 100 == 0 then synchronize ()
  done

let draw_point ((x, y), _ : point * color): unit =
  set_color black;
  fill_circle x y 2

let draw_points (): unit =
  List.iter draw_point ps

let _ =
  open_graph "";
  auto_synchronize false;
  resize_window window_width window_height;
  draw_voronoi @@ taxicab_distance;
  draw_points ();
  synchronize ();
  read_key ()
