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
  end

module Voro2dTree = VoroKdTree.Make(Element2D)
open Voro2dTree

module Future4 = Future.Make(struct
                              let process_limit = 4
                            end)
open Future4

type chunk = color array array

let window_width = 800
let window_height = 600
let amount_of_point = 100

type voro_config = { p: int;
                     width: int;
                     height: int;
                     amount_of_point: int }

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
        Voro2dTree.search_near_point distance (x, y) seedsTree
        |> BatOption.default black
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

let splitters : (point -> rect -> rect * rect ) array =
  [|VoroGeo.split_rect_vert;
    VoroGeo.split_rect_hor|]

let draw_tree (tree: seed kdnode): unit =
  let width = size_x () in
  let height = size_y () in
  let rec draw_tree_impl (node: seed kdnode) {position; size} (depth: int): unit =
    let rx, ry = position in
    let w, h = size in
    match node with
    | KdNode ((pivot, _), left, right) ->
       Graphics.set_color Graphics.black;
       Graphics.draw_rect rx ry w h;
       let axis = depth mod 2 in
       let splitter = splitters.(axis) in
       let (left_rect, right_rect) = splitter pivot @@ make_rect rx ry w h in
       draw_tree_impl left left_rect (depth + 1);
       draw_tree_impl right right_rect (depth + 1)
    | KdNil ->
       Graphics.set_color Graphics.black;
       Graphics.draw_rect rx ry w h
  in
  draw_tree_impl tree (make_rect 0 0 width height) 0

let update_config (config: voro_config)
                  (flag: string)
                  (value: string): voro_config =
  match flag with
  | "-p" -> { config with p = int_of_string value }
  | "-w" -> { config with width = int_of_string value }
  | "-h" -> { config with height = int_of_string value }
  | "-n" -> { config with amount_of_point = int_of_string value }
  | _ -> config

let parse_args (args: string list): voro_config =
  let default_config = { width = 800;
                         height = 600;
                         amount_of_point = 100;
                         p = 2 } in
  default_config

let _ =
  let config = Array.to_list Sys.argv |> parse_args in
  open_graph "";
  auto_synchronize false;
  resize_window window_width window_height;
  draw_voronoi @@ pnorm_distance config.p;
  draw_tree seedsTree;
  draw_points ();
  synchronize ();
  read_key ()
