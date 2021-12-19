open ContainersLabels
open Angstrom

include struct
  let a = 0
end

module A = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let input = "./lib/day_18/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 18A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 18B: %i\n%!" in

  ()
