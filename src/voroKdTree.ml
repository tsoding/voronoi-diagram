open Graphics
open VoroGeo
open VoroSeeds
open BatPervasives

type kdnode =
  | KdNode of seed * kdnode * kdnode
  | KdNil

type kdtree = kdnode

let k = 2

let accessors : ('a * 'a -> 'a) array = [|fst; snd|]

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

let search_near_point (point: point) (tree: kdtree): color option = None
