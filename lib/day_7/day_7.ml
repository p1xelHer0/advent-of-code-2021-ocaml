open ContainersLabels

let parse_ints input =
  List.hd input |> String.split_on_char ~by:',' |> List.map ~f:int_of_string

let distance a b = Int.abs (a - b)

module A = struct
  let solve l =
    let median = Aoc_2021.Util.median l in
    List.fold_left ~f:(fun accu n -> distance n median + accu) ~init:0 l

  let%test _ = solve [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] = 37
end

module B = struct
  let fuel_cost a b =
    let d = distance a b in
    d * succ d / 2

  let solve l =
    let mean = Aoc_2021.Util.mean l in
    let f mean' =
      List.fold_left ~f:(fun accu b -> fuel_cost b mean' + accu) ~init:0 l
    in
    min (f mean) (succ mean |> f)

  let%test _ = solve [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ] = 168
end

let run () =
  let input = "./lib/day_7/input" in

  let parsed_input = input |> Util.read_file |> parse_ints in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 7A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 7B: %i\n%!" in
  ()
