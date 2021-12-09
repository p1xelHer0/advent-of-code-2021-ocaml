open ContainersLabels

type seven_digit_display = {
  segments : string list;
  output : string list;
}

let parsers =
  [
    (* 14x developer - 14x arguments  - clean code is my passion *)
    Aoc_2021.Util.parse "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s"
      (fun s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 o1 o2 o3 o4 ->
        {
          segments = [ s1; s2; s3; s4; s5; s6; s7; s8; s9; s10 ];
          output = [ o1; o2; o3; o4 ];
        }
    );
  ]

let parse_seven_digit_display = Aoc_2021.Util.try_parse parsers

module A = struct
  let solve_aux sdd =
    sdd.output
    |> List.filter ~f:(fun s ->
           match String.length s with 2 | 3 | 4 | 7 -> true | _ -> false
       )
    |> List.length

  let solve l =
    l
    |> List.map ~f:parse_seven_digit_display
    |> List.map ~f:solve_aux
    |> Aoc_2021.Util.sum

  let%test _ =
    solve
      [
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe";
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc";
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg";
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb";
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea";
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb";
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe";
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef";
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb";
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";
      ]
    = 26
end

module B = struct
  let digit_of_segment = function
    | "abcefg" -> 0
    | "cf" -> 1
    | "acdeg" -> 2
    | "acdfg" -> 3
    | "bcdf" -> 4
    | "abdfg" -> 5
    | "abdefg" -> 6
    | "acf" -> 7
    | "abcdefg" -> 8
    | "abcdfg" -> 9
    | _ -> failwith "not a digit"

  let sort_segment s =
    String.to_seq s
    |> List.of_seq
    |> List.sort ~cmp:Char.compare
    |> List.to_seq
    |> String.of_seq

  let solve_aux sdd = sdd.output |> List.map ~f:sort_segment

  let segment_union a b =
    a ^ b
    |> String.to_seq
    |> List.of_seq
    |> Util.remove_duplicates
    |> List.sort ~cmp:Char.compare
    |> List.to_seq
    |> String.of_seq

  let segment_difference a b =
    let b' = b |> String.to_seq |> List.of_seq in
    let not_in_b x = not (List.mem x b') in
    a
    |> String.to_seq
    |> List.of_seq
    |> List.filter ~f:not_in_b
    |> List.to_seq
    |> String.of_seq

  let map_wiring l =
    let _one = l |> List.filter ~f:(fun s -> String.length s = 2) |> List.hd in
    let _four = l |> List.filter ~f:(fun s -> String.length s = 4) |> List.hd in
    let _seven =
      l |> List.filter ~f:(fun s -> String.length s = 3) |> List.hd
    in

    let a = segment_difference _seven _one in
    a

  let solve l =
    l
    |> List.map ~f:parse_seven_digit_display
    |> List.hd
    |> fun s -> map_wiring s.output

  (* let%test _ = *)
  (*   solve *)
  (*     [ *)
  (*       "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"; *)
  (*       "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"; *)
  (*       "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"; *)
  (*       "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"; *)
  (*       "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"; *)
  (*       "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"; *)
  (*       "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"; *)
  (*       "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"; *)
  (*       "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"; *)
  (*       "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"; *)
  (*     ] *)
  (*   = 61229 *)
end

let run () =
  let input = "./lib/day_8/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 8A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 8B: %s\n%!" in
  ()
