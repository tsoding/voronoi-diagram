open Graphics
open VoroGeo
open VoroSeeds
open BatPervasives

type kdnode =
  | KdNode of seed * kdnode * kdnode
  | KdNil

type kdtree = kdnode

let k = 2

let split_rect_vert (x, y: point) (rx, ry, w, h: rect): rect * rect =
  let lw = abs (rx - x) in
  ((rx, ry, lw, h),
   (x, ry, w - lw, h))

let split_rect_hor ((x, y): point) ((rx, ry, w, h): rect): rect * rect =
  let lh = abs (ry - y) in
  ((rx, ry, w, lh),
   (rx, y, w, h - lh))

let accessors : ('a * 'a -> 'a) array = [|fst; snd|]
let hyperpivot : (point -> point -> point) array =
  [| (fun (sx, sy) (px, py) -> px, sy);
     (fun (sx, sy) (px, py) -> sx, py) |]
let splitters : (point -> rect -> rect * rect ) array = [|split_rect_vert; split_rect_hor|]

let compare_with (f: 'a -> 'b) (a: 'a) (b: 'a): int =
  compare (f a) (f b)

let build (seeds: seed list): kdtree =
  let rec build_impl (seeds: seed list) (seeds_length: int) (depth: int): kdnode =
    let rest xs =
      match xs with
      | [] -> []
      | _ -> List.tl xs
    in
    match seeds with
    | [seed] -> KdNode (seed, KdNil, KdNil)
    | [] -> KdNil
    | _ ->
       let axis = depth mod k in
       let sorted_seeds = List.sort (compare_with (accessors.(axis) % fst)) seeds in
       let seeds_half_length = seeds_length / 2 in
       let left_seeds = BatList.take seeds_half_length sorted_seeds in
       let median_seed = sorted_seeds
                         |> BatList.drop seeds_half_length
                         |> List.hd in
       let right_seeds = sorted_seeds
                         |> BatList.drop seeds_half_length
                         |> rest in
       KdNode (median_seed,
               build_impl left_seeds seeds_half_length (depth + 1),
               build_impl right_seeds (seeds_half_length - 1) (depth + 1))
  in
  build_impl seeds (List.length seeds) 0

let print_tree (tree: kdtree): unit =
  let rec print_tree_impl (node: kdnode) (depth: int): unit =
    match node with
    | KdNode (((x, y), _), left, right) ->
       print_string @@ String.make depth ' ';
       Printf.printf "(%d, %d)\n" x y;
       print_tree_impl left (depth + 1);
       print_tree_impl right (depth + 1)
    | KdNil -> ()
  in
  print_tree_impl tree 0


let draw_tree (tree: kdtree): unit =
  let width = size_x () in
  let height = size_y () in
  let rec draw_tree_impl (node: kdnode) (rx, ry, w, h: rect) (depth: int): unit =
    match node with
    | KdNode ((pivot, _), left, right) ->
       Graphics.set_color Graphics.black;
       Graphics.draw_rect rx ry w h;
       let axis = depth mod k in
       let splitter = splitters.(axis) in
       let (left_rect, right_rect) = splitter pivot (rx, ry, w, h) in
       draw_tree_impl left left_rect (depth + 1);
       draw_tree_impl right right_rect (depth + 1)
    | KdNil ->
       Graphics.set_color Graphics.black;
       Graphics.draw_rect rx ry w h
  in
  draw_tree_impl tree (0, 0, width, height) 0

let search_near_point (point: point) (tree: kdtree): color option =
  let rec search_near_point_impl (node: kdnode) (depth: int): seed option =
    match node with
    | KdNode ((pivot, _), left, right) ->
       let best_result = search_near_point_impl left (depth + 1) in
       (match best_result with
        | Some (best_point, best_color) ->
           let axis = depth mod k in
           let best_distance = VoroGeo.euclidean_distance point best_point in
           let pivot_distance = VoroGeo.euclidean_distance point (hyperpivot.(axis) point pivot) in
           if pivot_distance < best_distance
           then let best_right_result = search_near_point_impl right (depth + 1) in
                (match (best_result, best_right_result) with
                 | (Some l, Some r) -> let left_distance = VoroGeo.euclidean_distance point (fst l) in
                                       let right_distance = VoroGeo.euclidean_distance point (fst r) in
                                       if left_distance < right_distance
                                       then Some l
                                       else Some r
                 | (Some l, None) -> Some l
                 | (None, Some r) -> Some r
                 | _ -> None)
           else best_result
        | None -> None)
    | KdNil -> None
  in
  BatOption.map snd (search_near_point_impl tree 0)
