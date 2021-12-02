type direction =
  | Forward
  | Down
  | Up

and command = {
  direction : direction;
  value : int;
}

let direction_of_string = function
  | "forward" -> Forward
  | "down" -> Down
  | "up" -> Up
  | _ -> failwith "invalid input data"

let parsers =
  [
    Aoc_2021.Util.parse "%s %d" (fun direction value ->
        { direction = direction_of_string direction; value }
    );
  ]

let parse_direction = Aoc_2021.Util.try_parse parsers

module A = struct
  type position = {
    depth : int;
    horizontal : int;
  }

  let next_position position command =
    match command with
    | { direction; value } -> (
        match direction with
        | Forward -> { position with horizontal = position.horizontal + value }
        | Down -> { position with depth = position.depth + value }
        | Up -> { position with depth = position.depth - value }
      )

  let solve l =
    List.fold_left next_position { depth = 0; horizontal = 0 } l
    |> fun p -> p.depth * p.horizontal

  let%test _ =
    solve
      (List.map parse_direction
         [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]
      )
    = 150
end

module B = struct
  type position = {
    depth : int;
    horizontal : int;
    aim : int;
  }

  let next_position position command =
    match command with
    | { direction; value } -> (
        match direction with
        | Forward ->
            {
              position with
              horizontal = position.horizontal + value;
              depth = position.depth + (value * position.aim);
            }
        | Down -> { position with aim = position.aim + value }
        | Up -> { position with aim = position.aim - value }
      )

  let solve l =
    List.fold_left next_position { depth = 0; horizontal = 0; aim = 0 } l
    |> fun p -> p.depth * p.horizontal

  let%test _ =
    solve
      (List.map parse_direction
         [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]
      )
    = 900
end

let run () =
  let input = "./lib/day_2/input" in

  let parsed_input = input |> Util.read_file |> List.map parse_direction in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 1A: %d\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 1B: %d\n%!" in
  ()
