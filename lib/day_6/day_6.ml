open ContainersLabels

let parse_ints input =
  List.hd input |> String.split_on_char ~by:',' |> List.map ~f:int_of_string

module A = struct
  (* slööööööw *)
  let rec grow n school =
    if n <= 0
    then school
    else
      let grow_aux school fish =
        match fish with
        | 0 -> List.append [ 6; 8 ] school
        | x -> List.append [ x - 1 ] school
      in
      grow (n - 1) (List.fold_left ~f:grow_aux ~init:[] school)

  let solve l = grow 80 l |> List.length

  let%test _ = solve [ 3; 4; 3; 1; 2 ] = 5934
end

module B = struct
  let fish_to_index l =
    let school = Array.make 9 0 in
    List.iter ~f:(fun fish -> school.(fish) <- school.(fish) + 1) l;
    Array.to_list school

  let grow = function
    | [ z; a; b; c; d; e; f; g; h ] -> [ a; b; c; d; e; f; g + z; h; z ]
    | _ -> []

  let solve' n l =
    let rec solve_aux n l = if n = 0 then l else solve_aux (n - 1) (grow l) in
    l |> fish_to_index |> solve_aux n |> List.fold_left ~f:( + ) ~init:0

  let solve l = solve' 256 l

  let%test _ = solve [ 3; 4; 3; 1; 2 ] = 26984457539
end

let run () =
  let input = "./lib/day_6/input" in

  let parsed_input = input |> Util.read_file |> parse_ints in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 6A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 6B: %i\n%!" in
  ()
