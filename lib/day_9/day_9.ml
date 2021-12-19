open ContainersLabels

let parse l =
  let parse_aux l =
    l
    |> String.to_seq
    |> List.of_seq
    |> List.map ~f:(fun c -> int_of_char c - Char.code '0')
    |> Array.of_list
  in
  l |> Array.of_list |> Array.map ~f:parse_aux

let neighbours (i, j) ~matrix =
  let m_height = Array.length matrix in
  let m_width = Array.length matrix.(0) in
  let within_matrix (i, j) = i >= 0 && j >= 0 && i < m_height && j < m_width in
  [ (i, succ j); (i, pred j); (succ i, j); (pred i, j) ]
  |> List.filter ~f:within_matrix

let low_point (i, j) ~matrix =
  let n = neighbours ~matrix (i, j) in
  n |> List.for_all ~f:(fun (i', j') -> matrix.(i).(j) < matrix.(i').(j'))

module A = struct
  let solve l =
    let matrix = l |> parse in
    let low_points = ref [] in
    for i = 0 to pred @@ Array.length matrix do
      for j = 0 to pred @@ Array.length matrix.(0) do
        if low_point ~matrix (i, j)
        then
          let height = matrix.(i).(j) in
          low_points := height :: !low_points
        else ()
      done
    done;
    !low_points |> List.fold_left ~f:(fun accu x -> accu + succ x) ~init:0

  let%test _ =
    solve
      [ "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" ]
    = 15
end

module B = struct
  let solve l =
    let _matrix = l |> parse in
    1

  (* let%test _ = *)
  (*   solve *)
  (*     [ "2199943210"; "3987894921"; "9856789892"; "8767896789"; "9899965678" ] *)
  (*   = 1134 *)
end

let run () =
  let input = "./lib/day_9/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 9A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 9B: %d\n%!" in
  ()
