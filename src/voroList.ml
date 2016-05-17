let group (n: int) (xs: 'a list): 'a list list =
  let rec group_impl (xs: 'a list) (acc: 'a list list): 'a list list =
    match xs with
    | [] -> acc
    | _ -> let rest = (BatList.drop n xs) in
           let group = (BatList.take n xs) in
           group_impl rest (group :: acc)
  in
  List.rev @@ group_impl xs [];
