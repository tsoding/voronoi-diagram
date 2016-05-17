open Graphics
open Future
open VoroGeo
open VoroSeeds
open Set

module Element2D =
  struct
    type elt = seed
    let k = 2

    let axis_get (idx: int) (point, _: elt): int =
      match idx with
      | 0 -> fst point
      | 1 -> snd point
      | _ -> failwith "Khooy"

    let as_string ((x, y), _: elt): string =
      String.concat "" ["(";
                        string_of_int x;
                        ", ";
                        string_of_int y;
                        ")"]

    let draw {position; size}: unit =
      let x, y = position in
      let w, h = size in
      Graphics.set_color Graphics.black;
      Graphics.draw_rect x y w h
  end

module Voro2dTree = VoroKdTree.Make(Element2D)
module Future4 = Future.Make(struct
                              let process_limit = 4
                            end)
open Future4

type chunk = color array array

let window_width = 800
let window_height = 600
let amount_of_point = 100

let seeds: seed list =
  generate_seeds { position = (0, 0);
                   size = (window_width, window_height) }
                 amount_of_point

let seedsTree: seed Voro2dTree.kdnode =
  Voro2dTree.build seeds

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
      let color =
        match Voro2dTree.search_near_point (x, y) seedsTree with
        | Some color -> color
        | None -> black
      in
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
  List.iter draw_point seeds

let _ =
  let p = (match Array.to_list Sys.argv with
           | _ :: valueOfP :: _ -> int_of_string valueOfP
           | _ -> 2) in
  open_graph "";
  auto_synchronize false;
  resize_window window_width window_height;
  draw_voronoi @@ pnorm_distance p;
  Voro2dTree.draw_tree seedsTree;
  draw_points ();
  synchronize ();
  read_key ()
