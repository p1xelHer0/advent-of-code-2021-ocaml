open ContainersLabels

let parse l =
  let numbers =
    l |> List.hd |> String.split_on_char ~by:',' |> List.map ~f:int_of_string
  in
  let parse_board l =
    l
    |> List.tl
    |> List.map ~f:(fun line ->
           String.split_on_char ~by:' ' line
           |> List.filter ~f:(fun s -> s |> String.is_empty |> not)
           |> List.map ~f:(fun s -> (int_of_string s, false))
           |> Array.of_list
       )
    |> Array.of_list
  in
  let boards =
    List.sublists_of_len ~len:6 (List.tl l) |> List.map ~f:parse_board
  in
  (numbers, boards)

module A = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let input = "./lib/day_20/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 20A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 20B: %i\n%!" in

  ()
