open ContainersLabels

module Coordinate = struct
  type t = {
    x : int;
    y : int;
    value : int;
  }

  let make ~x ~y value = { x; y; value }

  let compare t1 t2 =
    match compare t1.x t2.x with 0 -> compare t1.y t2.y | _ as x -> x

  let pp t =
    "(("
    ^ string_of_int t.x
    ^ ","
    ^ string_of_int t.y
    ^ "): "
    ^ string_of_int t.value
    ^ ")"
end

module PQ = Psq.Make (Coordinate) (Int)
module CMap = CCMap.Make (Coordinate)

let print_matrix matrix =
  Array.iter
    ~f:(fun a ->
      Array.iter ~f:print_int a;
      print_newline ()
    )
    matrix;
  print_newline ()

let parse l =
  let parse_aux l =
    l
    |> String.to_list
    |> List.map ~f:(fun c -> int_of_char c - Char.code '0')
    |> Array.of_list
  in
  l |> Array.of_list |> Array.map ~f:parse_aux

let neighbour ~x ~y matrix =
  let w = Array.length matrix.(0) in
  let h = Array.length matrix in
  let bounds (x, y) = x >= 0 && y >= 0 && x < h && y < w in
  [ (x, succ y); (succ x, y); (x, pred y); (pred x, y) ]
  |> List.filter ~f:bounds

let dijkstra ~source ~target matrix =
  let pq = ref (PQ.add source 0 PQ.empty) in
  let distance = ref (CMap.add source 0 CMap.empty) in

  for y = 0 to pred @@ Array.length matrix do
    for x = 0 to pred @@ Array.length matrix.(0) do
      let c = Coordinate.make ~x ~y matrix.(y).(x) in
      distance := CMap.add c Int.max_int !distance
    done
  done;

  while not (PQ.is_empty !pq) do
    let (u, u_dist), npq =
      match PQ.pop !pq with
      | None -> failwith "this loop only runs when pq isn't empty..."
      | Some (c, npq) -> (c, npq)
    in
    let () = pq := npq in
    let neighbours = Coordinate.(neighbour ~x:u.x ~y:u.y matrix) in

    let check_neighbour (x, y) =
      let v = Coordinate.make ~x ~y matrix.(y).(x) in
      let v_dist = Coordinate.(v.value) in

      match CMap.get v !distance with
      | None -> ()
      | Some old_distance ->
          let new_distance = u_dist + v_dist in
          if old_distance > new_distance
          then
            let () = distance := CMap.add v new_distance !distance in
            let () = pq := PQ.add v new_distance !pq in
            ()
          else ()
    in
    let () = List.iter ~f:check_neighbour neighbours in
    ()
  done;
  CMap.get target !distance

let test_data =
  [
    "1163751742";
    "1381373672";
    "2136511328";
    "3694931569";
    "7463417111";
    "1319128137";
    "1359912421";
    "3125421639";
    "1293138521";
    "2311944581";
  ]

module A = struct
  let solve l =
    let matrix = parse l in

    let target =
      let x = pred @@ Array.length matrix.(0) in
      let y = pred @@ Array.length matrix in
      Coordinate.make ~x ~y matrix.(y).(x)
    in

    let source = Coordinate.make ~x:0 ~y:0 matrix.(0).(0) in
    match dijkstra ~source ~target matrix with
    | Some d -> d
    | None -> failwith "oops"

  let%test _ = solve test_data = 40
end

module B = struct
  let generate_real_cave matrix =
    let cave_multiplier = 5 in
    let h = Array.length matrix in
    let w = Array.length matrix.(0) in

    let new_matrix =
      Array.make_matrix ~dimx:(h * cave_multiplier) ~dimy:(w * cave_multiplier)
        0
    in

    for t1 = 0 to pred @@ cave_multiplier do
      for t2 = 0 to pred @@ cave_multiplier do
        for y = 0 to pred @@ h do
          for x = 0 to pred @@ w do
            new_matrix.(y + (t1 * h)).(x + (t2 * w)) <-
              ((matrix.(y).(x) + t1 + t2 - 1) mod 9) + 1
          done
        done
      done
    done;

    new_matrix

  let solve l =
    let matrix = l |> parse |> generate_real_cave in

    let target =
      let x = pred @@ Array.length matrix.(0) in
      let y = pred @@ Array.length matrix in
      Coordinate.make ~x ~y matrix.(y).(x)
    in

    let source = Coordinate.make ~x:0 ~y:0 matrix.(0).(0) in
    match dijkstra ~source ~target matrix with
    | Some d -> d
    | None -> failwith "oops"

  let%test _ = solve test_data = 315
end

let run () =
  let input = "./lib/day_15/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 15A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 15B: %i\n%!" in
  ()
