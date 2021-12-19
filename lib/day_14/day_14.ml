open ContainersLabels

type growth = {
  opening : Char.t;
  closing : Char.t;
  value : Char.t;
}

type instruction =
  | Reaction of growth
  | Polymer of string

let parsers =
  Aoc_2021.Util.
    [
      parse "%s" (fun s -> Polymer s);
      parse "%c%c -> %c" (fun opening closing value ->
          Reaction { opening; closing; value }
      );
    ]
  

let parse_instructions = Aoc_2021.Util.try_parse parsers

module A = struct
  let solve l =
    let instructions = List.map ~f:parse_instructions l in
    let _reactions =
      instructions
      |> List.filter_map ~f:(function Reaction r -> Some r | Polymer _ -> None)
    in
    let _polymer =
      instructions
      |> List.filter_map ~f:(function Reaction _ -> None | Polymer t -> Some t)
      |> List.hd
      |> String.to_list
    in

    (* List.iter ~f:print_char polymer; *)
    (* print_endline ""; *)
    7

  let%test _ =
    solve
      [
        "NNCB";
        "CH -> B";
        "HH -> N";
        "CB -> H";
        "NH -> C";
        "HB -> C";
        "HC -> B";
        "HN -> C";
        "NN -> C";
        "BH -> H";
        "NC -> B";
        "NB -> B";
        "BN -> B";
        "BB -> N";
        "BC -> B";
        "CC -> N";
        "CN -> C";
      ]
    = 7
end

module B = struct
  let solve _l = 1

  let%test _ = solve [] = 1
end

let run () =
  let input = "./lib/day_14/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 14A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 14B: %i\n%!" in

  ()
