open ContainersLabels

let parse_input l = l |> List.map ~f:String.to_list

type syntax =
  | Paren
  | Bracket
  | Curly
  | Neq

type syntax' =
  | Close of syntax
  | Open of syntax

let parse_open = function
  | '(' -> Some (Open Paren)
  | '[' -> Some (Open Bracket)
  | '{' -> Some (Open Curly)
  | '<' -> Some (Open Neq)
  | _ -> None

let parse_close = function
  | ')' -> Some (Close Paren)
  | ']' -> Some (Close Bracket)
  | '}' -> Some (Close Curly)
  | '>' -> Some (Close Neq)
  | _ -> None

let parse_char c =
  match parse_open c with
  | Some _ as t -> t
  | None -> (
      match parse_close c with None -> None | Some _ as t -> t
    )

module A = struct
  let score = function
    | Paren -> 3
    | Bracket -> 57
    | Curly -> 1197
    | Neq -> 25137

  let rec solve_aux prev current =
    match current with
    | [] -> 0
    | ch :: ct -> (
        match ch with
        | Open _ as current_open -> solve_aux (current_open :: prev) ct
        | Close current_closed -> (
            match prev with
            | [] -> solve_aux [ ch ] ct
            | hd :: tl -> (
                match hd with
                | Open previous_open ->
                    if Stdlib.( = ) current_closed previous_open
                    then solve_aux tl ct
                    else score current_closed
                | Close _pc -> solve_aux prev ct
              )
          )
      )

  let solve l =
    l
    |> parse_input
    |> List.map ~f:(List.filter_map ~f:parse_char)
    |> List.map ~f:(solve_aux [])
    |> Aoc_2021.Util.sum

  let%test _ =
    solve
      [
        "[({(<(())[]>[[{[]{<()<>>";
        "[(()[<>])]({[<{<<[]>>(";
        "{([(<{}[<>[]}>{[]{[(<()>";
        "(((({<>}<{<{<>}{[]{[]{}";
        "[[<[([]))<([[{}[[()]]]";
        "[{[{({}]{}}([{[{{{}}([]";
        "{<[[]]>}<{[{[{[]{()[[[]";
        "[<(<(<(<{}))><([]([]()";
        "<{([([[(<>()){}]>(<<{{";
        "<{([{{}}[<[[[<>{}]]]>[]]";
      ]
    = 26397
end

module B = struct
  let score = function
    | Open s | Close s -> (
        match s with Paren -> 1 | Bracket -> 2 | Curly -> 3 | Neq -> 4
      )

  let rec solve_aux prev current =
    match current with
    | [] -> prev
    | ch :: ct -> (
        match ch with
        | Open _ as current_open -> solve_aux (current_open :: prev) ct
        | Close current_closed -> (
            match prev with
            | [] -> solve_aux [ ch ] ct
            | hd :: tl -> (
                match hd with
                | Open previous_open ->
                    if Stdlib.( = ) current_closed previous_open
                    then solve_aux tl ct
                    else []
                | Close _pc -> solve_aux prev ct
              )
          )
      )

  let a accu s = (accu * 5) + score s

  let solve l =
    l
    |> parse_input
    |> List.map ~f:(List.filter_map ~f:parse_char)
    |> List.map ~f:(solve_aux [])
    |> List.map ~f:(List.fold_left ~f:a ~init:0)
    |> List.filter ~f:(fun x -> x > 0)
    |> List.sort ~cmp:Int.compare
    |> Array.of_list
    |> fun a -> a.(Array.length a / 2)

  let%test _ =
    solve
      [
        "[({(<(())[]>[[{[]{<()<>>";
        "[(()[<>])]({[<{<<[]>>(";
        "{([(<{}[<>[]}>{[]{[(<()>";
        "(((({<>}<{<{<>}{[]{[]{}";
        "[[<[([]))<([[{}[[()]]]";
        "[{[{({}]{}}([{[{{{}}([]";
        "{<[[]]>}<{[{[{[]{()[[[]";
        "[<(<(<(<{}))><([]([]()";
        "<{([([[(<>()){}]>(<<{{";
        "<{([{{}}[<[[[<>{}]]]>[]]";
      ]
    = 288957
end

let run () =
  let input = "./lib/day_10/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 10A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 10B: %d\n%!" in
  ()
