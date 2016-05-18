open Graphics
open VoroGeo
open VoroSeeds
open BatPervasives

module type ElementType =
  sig
    type elt
    val k: int
    val make: int list -> elt
    val axis_get: int -> elt -> int
    val as_string: elt -> string
    val distance: elt -> elt -> float
  end

module type Kd =
  sig
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil
    type elt
    val build : elt list -> elt kdnode
    val search_near_point : distance_function -> point -> seed kdnode -> color option
    val search_near_point_general : elt -> elt kdnode -> elt option
    val print_tree : elt kdnode -> unit
  end

module Make(Elt: ElementType) =
  struct
    type 'a kdnode =
      | KdNode of 'a * 'a kdnode * 'a kdnode
      | KdNil

    type elt = Elt.elt

    let k = 2

    let accessors : ('a * 'a -> 'a) array = [|fst; snd|]
    let hyperpivot : (point -> point -> point) array =
      [| (fun (sx, sy) (px, py) -> px, sy);
         (fun (sx, sy) (px, py) -> sx, py) |]

    let hyper (axis: int) (search_elt: elt) (pivot_elt: elt): elt =
      let search_point = Array.init Elt.k @@ (fun x -> Elt.axis_get x search_elt) in
      Array.set search_point axis @@ Elt.axis_get axis pivot_elt;
      Elt.make @@ Array.to_list search_point

    let compare_with (f: 'a -> 'b) (a: 'a) (b: 'a): int =
      compare (f a) (f b)

    let build (elts: elt list): elt kdnode =
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

    let print_tree (tree: elt kdnode): unit =
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


    let closer_seed (distance: distance_function)
                    (search_point: point)
                    (seed1_point, _ as seed1: seed)
                    (seed2_point, _ as seed2: seed): seed =
      let seed1_distance = distance search_point seed1_point in
      let seed2_distance = distance search_point seed2_point in
      if seed1_distance < seed2_distance
      then seed1
      else seed2

    let closer_elt (search_elt: elt) (elt1: elt) (elt2: elt): elt =
      let elt1_distance = Elt.distance search_elt elt1 in
      let elt2_distance = Elt.distance search_elt elt2 in
      if elt1_distance < elt2_distance
      then elt1
      else elt2

    let search_near_point (distance: distance_function)
                          (search_point: point)
                          (tree: seed kdnode): color option =
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
             |> BatOption.map @@ (closer_seed distance search_point pivot_seed)
             |> BatOption.default pivot_seed
           in

           let hyperpivot_point = hyperpivot.(axis) search_point pivot_point in
           let hyperpivot_distance = distance search_point hyperpivot_point in
           let best_distance = distance search_point best_point in
           let probably_better_seed = if hyperpivot_distance < best_distance
                                      then search_near_point_impl opposite_branch (depth + 1)
                                      else None in

           let result_seed = probably_better_seed
                             |> BatOption.map (closer_seed distance search_point best_seed)
                             |> BatOption.default best_seed
           in

           Some result_seed
        | KdNil -> None
      in
      BatOption.map snd (search_near_point_impl tree 0)

    let search_near_point_general (search_elt: elt)
                                  (tree: elt kdnode): elt option =
      let rec search_near_point_general_impl (node: elt kdnode) (depth: int): elt option =
        match node with
        | KdNode (pivot_elt, left, right) ->
           let axis = depth mod Elt.k in
           let search_axis = Elt.axis_get axis search_elt in
           let pivot_axis = Elt.axis_get axis pivot_elt in
           let (next_branch, opposite_branch) =
             if search_axis < pivot_axis
             then (left, right)
             else (right, left)
           in

           let best_elt =
             search_near_point_general_impl next_branch (depth + 1)
             |> BatOption.map @@ (closer_elt search_elt pivot_elt)
             |> BatOption.default pivot_elt
           in

           let hyper_elt = hyper axis search_elt pivot_elt in
           let hyper_distance = Elt.distance search_elt hyper_elt in
           let best_distance = Elt.distance search_elt best_elt in
           let probably_better_elt = if hyper_distance < best_distance
                                     then search_near_point_general_impl opposite_branch (depth + 1)
                                     else None in

           let result_elt = probably_better_elt
                            |> BatOption.map (closer_elt search_elt best_elt)
                            |> BatOption.default best_elt
           in

           Some result_elt
        | KdNil -> None
      in
      search_near_point_general_impl tree 0
  end
