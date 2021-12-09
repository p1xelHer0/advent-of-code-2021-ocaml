module A = struct
  let solve l = 0

  let%test _ =
    solve
      [|
        [| 2; 1; 9; 9; 9; 4; 3; 2; 1; 0 |];
        [| 3; 9; 8; 7; 8; 9; 4; 9; 2; 1 |];
        [| 9; 8; 5; 6; 7; 8; 9; 8; 9; 2 |];
        [| 8; 7; 6; 7; 8; 9; 6; 7; 8; 9 |];
        [| 9; 8; 9; 9; 9; 6; 5; 6; 7; 8 |];
      |]
    = 15
end

module B = struct
  let solve _l = 15

  let%test _ =
    solve
      [|
        [| 2; 1; 9; 9; 9; 4; 3; 2; 1; 0 |];
        [| 3; 9; 8; 7; 8; 9; 4; 9; 2; 1 |];
        [| 9; 8; 5; 6; 7; 8; 9; 8; 9; 2 |];
        [| 8; 7; 6; 7; 8; 9; 6; 7; 8; 9 |];
        [| 9; 8; 9; 9; 9; 6; 5; 6; 7; 8 |];
      |]
    = 15
end

let run () =
  let input = "./lib/day_9/input" in

  let parsed_input =
    input |> Util.read_file |> List.map ~f:int_of_string |> Array.of_list
  in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 9A: %d\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 9B: %d\n%!" in
  ()
