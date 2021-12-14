open ContainersLabels

type octopus = {
  energy : int;
  has_flashed : bool;
}

let print_octopus_array matrix =
  Array.iter
    ~f:(fun a ->
      Array.iter ~f:(fun o -> print_int o.energy) a;
      print_newline ()
    )
    matrix;
  print_newline ()

let make n = { energy = n; has_flashed = false }

let parse l =
  let parse_aux l =
    l
    |> String.to_list
    |> List.map ~f:(fun c -> make (int_of_char c - Char.code '0'))
    |> Array.of_list
  in
  l |> Array.of_list |> Array.map ~f:parse_aux

let adjacent (i, j) ~matrix =
  let w = Array.length matrix.(0) in
  let h = Array.length matrix in
  let bounds (i, j) = i >= 0 && j >= 0 && i < h && j < w in
  [
    (i, succ j);
    (i, pred j);
    (succ i, j);
    (pred i, j);
    (succ i, succ j);
    (pred i, pred j);
    (succ i, pred j);
    (pred i, succ j);
  ]
  |> List.filter ~f:bounds

let rec flash (i, j) ~matrix ~flashes =
  if not @@ matrix.(i).(j).has_flashed
  then (
    matrix.(i).(j) <- { (matrix.(i).(j)) with has_flashed = true };
    flashes := succ !flashes;
    let adjacent_octopus = adjacent ~matrix (i, j) in

    adjacent_octopus
    |> List.iter ~f:(fun (i', j') ->
           let octopus = matrix.(i').(j') in
           matrix.(i').(j') <- { octopus with energy = succ octopus.energy }
       );

    adjacent_octopus
    |> List.filter ~f:(fun (i', j') ->
           matrix.(i').(j').energy > 9 && not matrix.(i').(j').has_flashed
       )
    |> List.iter ~f:(flash ~matrix ~flashes)
  )
  else ()

let step matrix =
  let flashes = ref 0 in

  Array.iter
    ~f:(fun os ->
      Array.iteri
        ~f:(fun i octopus ->
          os.(i) <- { octopus with energy = succ @@ octopus.energy }
        )
        os
    )
    matrix;

  for i = 0 to pred @@ Array.length matrix do
    for j = 0 to pred @@ Array.length matrix.(0) do
      if matrix.(i).(j).energy > 9 && not matrix.(i).(j).has_flashed
      then flash (i, j) ~matrix ~flashes
      else ()
    done
  done;

  Array.iter
    ~f:(fun os ->
      Array.iteri
        ~f:(fun i octopus -> if octopus.has_flashed then os.(i) <- make 0)
        os
    )
    matrix;
  !flashes

module A = struct
  let solve_aux matrix ~times =
    let flashes = ref 0 in
    for _ = 1 to times do
      flashes := !flashes + step matrix
    done;
    !flashes

  let solve l = l |> parse |> solve_aux ~times:100

  let%test _ =
    solve
      [
        "5483143223";
        "2745854711";
        "5264556173";
        "6141336146";
        "6357385478";
        "4167524645";
        "2176841721";
        "6882881134";
        "4846848554";
        "5283751526";
      ]
    = 1656
end

module B = struct
  let solve l =
    let matrix = l |> parse in
    OSeq.iterate 1 succ
    |> OSeq.map (fun n -> (n, step matrix))
    |> OSeq.find (fun (_n, flashes) -> flashes = 100)
    |> Option.get_exn_or "no simultaneous flash"
    |> fst
end

let run () =
  let input = "./lib/day_11/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 11A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 11B: %i\n%!" in
  ()
