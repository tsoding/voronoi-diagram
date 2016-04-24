open Graphics

let window_width = 800
let window_height = 600

let min_with (f: 'a -> 'b) (a: 'a) (b: 'a): 'a =
  if (f a) < (f b) then a else b

let ps: (int * int * color) list =
  [(1, 2, blue);
   (500, 400, green)]

let distance (x1, y1: int * int) (x2, y2: int * int): float =
  let dx = float_of_int (x1 - x2) in
  let dy = float_of_int (y1 - y2) in
  sqrt (dx *. dx +. dy *. dy)

let get_color (p: int * int): color =
  ps
  |> List.map (fun (x, y, color) -> (distance p (x, y), color))
  |> List.fold_left (min_with fst) (max_float, black)
  |> snd

let draw_voronoi (): unit =
  for y = 0 to window_height - 1 do
    for x = 0 to window_width - 1 do
      let color = get_color (x, y) in
      set_color color;
      fill_rect x y 1 1
    done
  done

let draw_point (x, y, _): unit =
  set_color black;
  fill_circle x y 5

let draw_points (): unit =
  List.iter draw_point ps

let _ = ()
