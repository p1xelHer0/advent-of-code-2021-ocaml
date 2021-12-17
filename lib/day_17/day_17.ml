open ContainersLabels

module TargetArea = struct
  type t = {
    min_x : int;
    max_x : int;
    min_y : int;
    max_y : int;
  }

  let make x1 x2 y1 y2 = { min_x = x1; max_x = x2; min_y = y1; max_y = y2 }
end

let parsers =
  Aoc_2021.Util.
    [
      parse "target area: x=%i..%i, y=%i..%i" (fun x1 x2 y1 y2 ->
          TargetArea.make x1 x2 y1 y2
      );
    ]
  

let parse_paths = Aoc_2021.Util.try_parse parsers

let solve_aux l =
  let ta = parse_paths l in
  let TargetArea.{ min_x; max_x; min_y; max_y } = ta in

  let step ((x, y), (dx, dy)) =
    ((x + dx, y + dy), (dx - compare dx 0, pred dy))
  in

  let max_height_of_target velocity =
    let rec iter (pos, velocity) max_height =
      match step (pos, velocity) with
      | (x, y), _ when x > max_x || y < min_y -> None
      | (x, y), _ when x >= min_x && x <= max_x && y >= min_y && y <= max_y ->
          Some max_height
      | (x, y), new_velocity -> iter ((x, y), new_velocity) (max max_height y)
    in
    iter ((0, 0), velocity) 0
  in

  OSeq.(product (0 --^ succ max_x) (min_y --^ abs min_y))
  |> OSeq.map max_height_of_target
  |> OSeq.to_list
  |> List.filter_map ~f:(fun x -> x)

module A = struct
  let solve l = l |> solve_aux |> List.fold_left ~f:max ~init:Int.min_int

  let%test _ = solve "target area: x=20..30, y=-10..-5" = 45
end

module B = struct
  let solve l = l |> solve_aux |> List.length

  let%test _ = solve "target area: x=20..30, y=-10..-5" = 112
end

let run () =
  let input = "./lib/day_17/input" in

  let parsed_input = input |> Util.read_file |> List.hd in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 17A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 17B: %i\n%!" in
  ()
