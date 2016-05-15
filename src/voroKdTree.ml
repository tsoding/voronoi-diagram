open Graphics
open VoroGeo
open VoroSeeds
open BatPervasives

module type ElementType =
  sig
    type elt
    type dem
    val k: int
    val axis_get: elt -> int -> dem
    val compare: dem -> dem -> int
  end

module type Kd =
  sig
    type kdtree
    val build : seed list -> kdtree
    val search_near_point : point -> kdtree -> color option
    val print_tree : kdtree -> unit
    val draw_tree : kdtree -> unit
  end

module Make(Elt: ElementType): Kd =
  struct
    type kdnode =
      | KdNode of seed * kdnode * kdnode
      | KdNil

    type kdtree = kdnode

    let k = 2

    let accessors : ('a * 'a -> 'a) array = [|fst; snd|]
    let hyperpivot : (point -> point -> point) array =
      [| (fun (sx, sy) (px, py) -> px, sy);
         (fun (sx, sy) (px, py) -> sx, py) |]
    let splitters : (point -> rect -> rect * rect ) array = [|VoroGeo.split_rect_vert; VoroGeo.split_rect_hor|]

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
      let rec draw_tree_impl (node: kdnode) {position; size} (depth: int): unit =
        let rx, ry = position in
        let w, h = size in
        match node with
        | KdNode ((pivot, _), left, right) ->
           Graphics.set_color Graphics.black;
           Graphics.draw_rect rx ry w h;
           let axis = depth mod k in
           let splitter = splitters.(axis) in
           let (left_rect, right_rect) = splitter pivot @@ make_rect rx ry w h in
           draw_tree_impl left left_rect (depth + 1);
           draw_tree_impl right right_rect (depth + 1)
        | KdNil ->
           Graphics.set_color Graphics.black;
           Graphics.draw_rect rx ry w h
      in
      draw_tree_impl tree (make_rect 0 0 width height) 0

    let closer_seed (search_point: point)
                    (seed1_point, _ as seed1: seed)
                    (seed2_point, _ as seed2: seed): seed =
      let seed1_distance = VoroGeo.euclidean_distance search_point seed1_point in
      let seed2_distance = VoroGeo.euclidean_distance search_point seed2_point in
      if seed1_distance < seed2_distance
      then seed1
      else seed2

    let search_near_point (search_point: point) (tree: kdtree): color option =
      let rec search_near_point_impl (node: kdnode) (depth: int): seed option =
        match node with
        | KdNode ((pivot_point, pivot_color) as pivot_seed, left, right) ->
           let axis = depth mod k in
           let search_axis = accessors.(axis) search_point in
           let pivot_axis = accessors.(axis) pivot_point in
           let (next_branch, opposite_branch) =
             if search_axis < pivot_axis
             then (left, right)
             else (right, left)
           in
           (match search_near_point_impl next_branch (depth + 1) with
            | Some best_seed -> Some (closer_seed search_point best_seed pivot_seed)
            | None -> Some (pivot_point, pivot_color))
        | KdNil -> None
      in
      BatOption.map snd (search_near_point_impl tree 0)
  end
