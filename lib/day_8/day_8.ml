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
  module Segment : sig
    module Set : Set.S with type elt = Char.t

    type t = string

    val union : t -> t -> t

    val difference : t -> t -> t

    val intersection : t -> t -> t

    val partition : (t -> bool) -> t list -> t list * t list
  end = struct
    module Set = Set.Make (Char)

    let set_of_string s = s |> String.to_seq |> Set.of_seq

    type t = string

    let f g s1 s2 =
      let s1' = set_of_string s1 in
      let s2' = set_of_string s2 in
      g s1' s2' |> Set.to_seq |> String.of_seq

    let union s1 s2 = f Set.union s1 s2

    let difference s1 s2 = f Set.diff s1 s2

    let intersection s1 s2 = f Set.inter s1 s2

    let partition p l = List.partition ~f:p l
  end

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

  let map_wiring l =
    let segments_with_length length l =
      let keep_length c = String.length c = length in
      l |> List.filter ~f:keep_length
    in

    let one = segments_with_length 2 l |> List.hd in
    let _four = segments_with_length 4 l |> List.hd in
    let seven = segments_with_length 3 l |> List.hd in

    let _twoThreeFive = segments_with_length 5 l in
    let _zeroSixNine = segments_with_length 5 l in

    let a = Segment.difference seven one in

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
