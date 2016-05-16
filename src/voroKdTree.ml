open Graphics
open VoroGeo
open VoroSeeds
open BatPervasives

module type ElementType =
  sig
    type elt
    val k: int
    val axis_get: int -> elt -> int
    val as_string: elt -> string
  end

module type Kd =
  sig
    type 'a kdtree
    type elt
    val build : elt list -> elt kdtree
    val search_near_point : point -> seed kdtree -> color option
    val print_tree : elt kdtree -> unit
    val draw_tree : seed kdtree -> unit
  end

module Make(Elt: ElementType) =
  struct
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil

    type 'a kdtree = 'a kdnode
    type elt = Elt.elt

    let k = 2

    let accessors : ('a * 'a -> 'a) array = [|fst; snd|]
    let hyperpivot : (point -> point -> point) array =
      [| (fun (sx, sy) (px, py) -> px, sy);
         (fun (sx, sy) (px, py) -> sx, py) |]
    let splitters : (point -> rect -> rect * rect ) array = [|VoroGeo.split_rect_vert; VoroGeo.split_rect_hor|]

    let compare_with (f: 'a -> 'b) (a: 'a) (b: 'a): int =
      compare (f a) (f b)

    let build (elts: elt list): elt kdtree =
      let rec build_impl (elts: elt list) (elts_length: int) (depth: int): elt kdnode =
        let rest xs =
          match xs with
          | [] -> []
          | _ -> List.tl xs
        in
        match elts with
        | [elt] -> KdNode (elt, KdNil, KdNil)
        | [] -> KdNil
        | _ ->
           let axis = depth mod k in
           let sorted_elts = List.sort (compare_with (Elt.axis_get axis)) elts in
           let elts_half_length = elts_length / 2 in
           let left_elts = BatList.take elts_half_length sorted_elts in
           let median_elt = sorted_elts
                             |> BatList.drop elts_half_length
                             |> List.hd in
           let right_elts = sorted_elts
                             |> BatList.drop elts_half_length
                             |> rest in
           KdNode (median_elt,
                   build_impl left_elts elts_half_length (depth + 1),
                   build_impl right_elts (elts_half_length - 1) (depth + 1))
      in
      build_impl elts (List.length elts) 0

    let print_tree (tree: elt kdtree): unit =
      let rec print_tree_impl (node: elt kdnode) (depth: int): unit =
        match node with
        | KdNode (elt, left, right) ->
           print_string @@ String.make depth ' ';
           print_endline @@ Elt.as_string elt;
           print_tree_impl left (depth + 1);
           print_tree_impl right (depth + 1)
        | KdNil -> ()
      in
      print_tree_impl tree 0


    let draw_tree (tree: seed kdtree): unit =
      let width = size_x () in
      let height = size_y () in
      let rec draw_tree_impl (node: seed kdnode) {position; size} (depth: int): unit =
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

    let search_near_point (search_point: point) (tree: seed kdtree): color option =
      let rec search_near_point_impl (node: seed kdnode) (depth: int): seed option =
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
           let best_point, _ as best_seed =
             search_near_point_impl next_branch (depth + 1)
             |> BatOption.map @@ (closer_seed search_point pivot_seed)
             |> BatOption.default pivot_seed
           in

           let hyperpivot_point = hyperpivot.(axis) search_point pivot_point in
           let hyperpivot_distance = VoroGeo.euclidean_distance search_point hyperpivot_point in
           let best_distance = VoroGeo.euclidean_distance search_point best_point in
           let probably_better_seed = if hyperpivot_distance < best_distance
                                      then search_near_point_impl opposite_branch (depth + 1)
                                      else None in

           let result_seed = probably_better_seed
                             |> BatOption.map (closer_seed search_point best_seed)
                             |> BatOption.default best_seed
           in

           Some result_seed
        | KdNil -> None
      in
      BatOption.map snd (search_near_point_impl tree 0)
  end
