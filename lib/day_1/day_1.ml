module A = struct
  let rec solve_aux accu l =
    match l with
    | [] | [ _ ] -> accu
    | x1 :: x2 :: tl ->
        let accu' = if x2 > x1 then accu + 1 else accu in
        solve_aux accu' (x2 :: tl)

  let solve = solve_aux 0

  let%test _ = solve [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ] = 7
end

module B = struct
  let sum = List.fold_left ( + ) 0

  let rec solve_aux accu l =
    match l with
    | x1 :: x2 :: x3 :: x4 :: tl ->
        let sliding_window_1 = sum [ x1; x2; x3 ] in
        let sliding_window_2 = sum [ x2; x3; x4 ] in
        let accu' =
          if sliding_window_2 > sliding_window_1 then accu + 1 else accu
        in
        solve_aux accu' (x2 :: x3 :: x4 :: tl)
    | _ -> accu

  let solve = solve_aux 0

  let%test _ = solve [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ] = 5
end

let run () =
  let input = "./lib/day_1/input" in

  let parsed_input = input |> Util.read_file |> List.map int_of_string in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 1A: %d\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 1B: %d\n%!" in
  ()
