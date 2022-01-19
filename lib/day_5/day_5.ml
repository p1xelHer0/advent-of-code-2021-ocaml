open ContainersLabels

module Coordinate = struct
  type t = {
    x : int;
    y : int;
  }

  let make ~x ~y = { x; y }

  let compare t1 t2 =
    match compare t1.x t2.x with 0 -> compare t1.y t2.y | _ as x -> x
end

module Line = struct
  type t = {
    start : Coordinate.t;
    finish : Coordinate.t;
  }

  let make start finish = { start; finish }

  let direction t =
    let open Coordinate in
    if t.start.x = t.finish.x
    then `Vertical
    else if t.start.y = t.finish.y
    then `Horizontal
    else `Diagonal

  let range t ~part =
    let range_x t =
      let open Coordinate in
      let fixed_coord = Coordinate.make ~x:t.start.x in
      Util.range t.start.y t.finish.y |> List.map ~f:(fun y -> fixed_coord ~y)
    in

    let range_y t =
      let open Coordinate in
      let fixed_coord = Coordinate.make ~y:t.start.y in
      Util.range t.start.x t.finish.x |> List.map ~f:(fun x -> fixed_coord ~x)
    in

    let range_diagonal t =
      let open Coordinate in
      let x_range = Util.range t.start.x t.finish.x in
      let y_range = Util.range t.start.y t.finish.y in
      List.combine x_range y_range
      |> List.map ~f:(fun (x, y) -> Coordinate.make ~x ~y)
    in

    match direction t with
    | `Horizontal -> range_y t
    | `Vertical -> range_x t
    | `Diagonal -> (
        match part with `Part_1 -> [] | `Part_2 -> range_diagonal t
      )
end

let parsers =
  [
    Aoc_2021.Util.parse "%i,%i -> %i,%i" (fun x1 y1 x2 y2 ->
        Line.make (Coordinate.make ~x:x1 ~y:y1) (Coordinate.make ~x:x2 ~y:y2)
    );
  ]

let parse_lines = Aoc_2021.Util.try_parse parsers

module Coordinates = Map.Make (Coordinate)

let bump_coord s coordinate =
  let value =
    match Coordinates.find_opt coordinate s with None -> 1 | Some x -> succ x
  in
  Coordinates.add coordinate value s

let solve_aux l ~part =
  List.map ~f:parse_lines l
  |> List.map ~f:(Line.range ~part)
  |> List.flatten
  |> List.fold_left ~f:bump_coord ~init:Coordinates.empty
  |> Coordinates.bindings
  |> List.filter ~f:(fun (_, v) -> v > 1)
  |> List.length

module A = struct
  let solve l = solve_aux l ~part:`Part_1

  let%test _ =
    solve
      [
        "0,9 -> 5,9";
        "8,0 -> 0,8";
        "9,4 -> 3,4";
        "2,2 -> 2,1";
        "7,0 -> 7,4";
        "6,4 -> 2,0";
        "0,9 -> 2,9";
        "3,4 -> 1,4";
        "0,0 -> 8,8";
        "5,5 -> 8,2";
      ]
    = 5
end

module B = struct
  let solve l = solve_aux l ~part:`Part_2

  let%test _ =
    solve
      [
        "0,9 -> 5,9";
        "8,0 -> 0,8";
        "9,4 -> 3,4";
        "2,2 -> 2,1";
        "7,0 -> 7,4";
        "6,4 -> 2,0";
        "0,9 -> 2,9";
        "3,4 -> 1,4";
        "0,0 -> 8,8";
        "5,5 -> 8,2";
      ]
    = 12
end

let run () =
  let input = "./lib/day_5/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 5A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 5B: %i\n%!" in
  ()
