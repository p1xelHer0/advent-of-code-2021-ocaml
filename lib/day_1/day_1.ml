module A = struct
  let solve input = input |> List.hd

  let%test _ = solve [ 10; 30; 50 ] = 10
end

module B = struct
  let solve input = input |> List.tl |> List.hd

  let%test _ = solve [ 10; 30; 50 ] = 30
end

let run () =
  let input = "./lib/day_1/input" in

  let parsed_input = input |> Util.read_file |> List.map ~f:int_of_string in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 1A: %d\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 1B: %d\n%!" in
  ()
