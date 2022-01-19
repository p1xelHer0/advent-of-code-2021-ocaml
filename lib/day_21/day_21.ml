open ContainersLabels

module Player = struct
  type t = {
    score : int;
    position : int;
  }

  let compare t1 t2 =
    match Int.compare t1.score t2.score with
    | 0 -> Int.compare t1.position t2.position
    | _ as c -> c
end

module GameState = struct
  type t = {
    player_1 : Player.t;
    player_2 : Player.t;
  }

  let compare t1 t2 =
    match Player.compare t1.player_1 t2.player_1 with
    | 0 -> Player.compare t1.player_2 t2.player_2
    | _ as c -> c
end

let parsers =
  [
    Aoc_2021.Util.parse "Player %i starting position: %i" (fun _id position ->
        Player.{ score = 0; position }
    );
  ]

let parse_lines = Aoc_2021.Util.try_parse parsers

module A = struct
  open Player

  let roll start =
    let wrap n = ((n - 1) mod 100) + 1 in
    OSeq.(start -- (start + 2)) |> OSeq.map wrap |> OSeq.sum

  let move player roll_start =
    let steps = roll roll_start in
    let position = ((player.position - 1 + steps) mod 10) + 1 in
    { position; score = player.score + position }

  let play p1 p2 =
    let rec loop p1 p2 roll turn =
      if p2.score >= 1000
      then p1.score * turn
      else
        let p1' = move p1 roll in
        loop p2 p1' (roll + 3) (turn + 3)
    in
    loop p1 p2 1 0

  let solve l =
    let players = l |> List.map ~f:parse_lines in
    let player_1 = List.hd players in
    let player_2 = List.tl players |> List.hd in
    play player_1 player_2

  let%test _ =
    solve [ "Player 1 starting position: 4"; "Player 2 starting position: 8" ]
    = 739785
end

module B = struct
  open Player
  (* open GameState *)

  (* let game_states = GS.empty *)

  let move player rolls =
    let steps = Aoc_2021.Util.sum rolls in
    let position = ((player.position - 1 + steps) mod 10) + 1 in
    { position; score = player.score + position }

  let play =
    OSeq.(product3 (1 -- 3) (1 -- 3) (1 -- 3))
    |> OSeq.fold_left
         (fun (w1, w2) (d1, d2, d3) -> 



           (w1 + d1 + d2 + d3, w2))
         (0, 0)

  let solve l =
    let players = l |> List.map ~f:parse_lines in
    let _player_1 = List.hd players in
    let _player_2 = List.tl players |> List.hd in
    fst play
end

let run () =
  let input = "./lib/day_21/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 21A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 21B: %i\n%!" in

  ()
