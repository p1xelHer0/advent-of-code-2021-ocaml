let parsers = [ Aoc_2021.Util.parse "%i" (fun x -> x) ]

let parse_lines = Aoc_2021.Util.try_parse parsers

module A = struct
  let solve l = List.hd l

  let%test _ = solve [ "oO" ] = "oO"
end

module B = struct
  let solve l = List.hd l

  let%test _ = solve [ "oO" ] = "oO"
end

let run () =
  let input = "./lib/day_6/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 6A: %s\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 6B: %s\n%!" in
  ()
