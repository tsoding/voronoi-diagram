open Graphics
open BatSeq
open Future
open VoroGeo

type chunk = color array array

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

let get_color (p: point) (distance: distance_function): color =
  ps
  |> List.map (fun ((x, y), color) -> (distance p (x, y), color))
  |> List.fold_left (min_with fst) (max_float, black)
  |> snd

let draw_chunk (x0, y0: int * int)
               (chunk: chunk): unit =
  let h = Array.length chunk in
  let w = Array.length chunk.(0) in
  for y = 0 to h - 1 do
    let row = Array.get chunk y in
    for x = 0 to w - 1 do
      let color = Array.get row x in
      set_color color;
      plot (x + x0) (y + y0)
    done
  done

let calc_chunk (distance: distance_function)
               (x0, y0, x1, y1: int * int * int * int): chunk =
  let w = x1 - x0 + 1 in
  let h = y1 - y0 + 1 in
  let chunk = Array.make_matrix h w black in
  for y = y0 to y1 do
    let row = Array.get chunk (y - y0) in
    for x = x0 to x1 do
      let color = get_color (x, y) distance in
      Array.set row (x - x0) color
    done
  done;
  chunk

let draw_voronoi (distance: distance_function): unit =
  let half_width = window_width / 2 in
  let half_height = window_height / 2 in
  let rects = [0, 0, half_width, half_height;
               half_width + 1, 0, window_width - 1, half_height;
               0, half_height + 1, half_width + 1, window_height - 1;
               half_width + 1, half_height + 1, window_width - 1, window_height - 1] in
  let chunks = rects |> List.map (fun rect -> future (calc_chunk distance) rect) in
  chunks
  |> List.map force
  |> List.combine rects
  |> List.iter (fun ((x, y, _, _), chunk) -> draw_chunk (x, y) chunk)

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
