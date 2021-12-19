open ContainersLabels

module Cave = struct
  module Set = Set.Make (String)

  type cave =
    | Big
    | Small
    | Start
    | End

  type t = {
    id : string;
    cave_type : cave;
    connected_to : Set.t;
  }

  let make id ~cave_type = { id; cave_type; connected_to = Set.empty }

  let connect_caves c1 c2 =
    ( { c1 with connected_to = Set.add c2.id c1.connected_to },
      { c2 with connected_to = Set.add c1.id c2.connected_to }
    )
end

let parsers =
  (* Aoc_2021.Util.( *)
  [ (* parse "start-%s" (fun _start _destination -> "a"); *)
    (* parse "end-%s" (fun _source _destination -> "a"); *)
    (* parse "%s-start" (fun _source _start -> "a"); *)
    (* parse "%s-%s" (fun _source _destination -> "a"); *) ]
(* ) *)

let parse_paths = Aoc_2021.Util.try_parse parsers

module A = struct
  let solve l = l |> List.map ~f:parse_paths |> List.hd |> String.length

  (* let%test _ = *)
  (*   solve [ "start-A"; "start-b"; "A-c"; "A-b"; "b-d"; "A-end"; "b-end " ] = 10 *)
end

(* module B = struct *)
(*   let solve l = List.hd *)
(* end *)

let run () =
  let input = "./lib/day_12/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 12A: %i\n%!" in

  (* let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 12B: %i\n%!" in *)
  ()
