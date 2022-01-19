open ContainersLabels

let parse l =
  let numbers =
    l |> List.hd |> String.split_on_char ~by:',' |> List.map ~f:int_of_string
  in
  let parse_board l =
    l
    |> List.tl
    |> List.map ~f:(fun line ->
           String.split_on_char ~by:' ' line
           |> List.filter ~f:(fun s -> s |> String.is_empty |> not)
           |> List.map ~f:(fun s -> (int_of_string s, false))
           |> Array.of_list
       )
    |> Array.of_list
  in
  let boards =
    List.sublists_of_len ~len:6 (List.tl l) |> List.map ~f:parse_board
  in
  (numbers, boards)

let print_board board =
  Array.iter
    ~f:(fun a ->
      Array.iter ~f:(fun (n, b) -> Printf.printf "(%i,%b)" n b) a;
      print_newline ()
    )
    board;
  print_newline ()

let mark n ~board =
  let h = pred @@ Array.length board in
  let w = pred @@ Array.length board.(0) in
  for y = 0 to h do
    for x = 0 to w do
      let x', _ = board.(y).(x) in
      if x' = n then board.(y).(x) <- (x', true)
    done
  done

let check_win (board : (int * bool) array array) =
  let h = Array.length board in
  let w = Array.length board.(0) in
  let check_row y =
    Array.fold_left ~f:(fun accu (_, v) -> accu && v) ~init:true board.(y)
  in
  let check_column x =
    Array.fold_left ~f:(fun accu a -> accu && (snd @@ a.(x))) ~init:true board
  in
  let win_of_row =
    OSeq.(0 --^ h) |> OSeq.filter (fun y -> check_row y) |> OSeq.to_list
  in
  let win_of_column =
    OSeq.(0 --^ w) |> OSeq.filter (fun x -> check_column x) |> OSeq.to_list
  in
  List.append win_of_column win_of_row |> List.length > 0

let score board =
  Array.to_seq board
  |> Seq.map Array.to_seq
  |> OSeq.flatten
  |> OSeq.filter_map (fun (x, marked) -> if not marked then Some x else None)
  |> OSeq.sum

module A = struct
  let solve l =
    let _numbers, boards = parse l in

    List.iter ~f:print_board boards;
    0

  let%test _ =
    solve
      [
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1";
        "";
        "22 13 17 11  0";
        " 8  2 23  4 24";
        "21  9 14 16  7";
        " 6 10  3 18  5";
        " 1 12 20 15 19";
        "";
        " 3 15  0  2 22";
        " 9 18 13 17  5";
        "19  8  7 25 23";
        "20 11 10 24  4";
        "14 21 16 12  6";
        "";
        "14 21 17 24  4";
        "10 16 15  9 19";
        "18  8 23 26 20";
        "22 11 13  6  5";
        " 2  0 12  3  7";
      ]
    = 4512
end

module B = struct
  let solve _l = 0

  let%test _ =
    solve
      [
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1";
        "";
        "22 13 17 11  0";
        " 8  2 23  4 24";
        "21  9 14 16  7";
        " 6 10  3 18  5";
        " 1 12 20 15 19";
        "";
        " 3 15  0  2 22";
        " 9 18 13 17  5";
        "19  8  7 25 23";
        "20 11 10 24  4";
        "14 21 16 12  6";
        "";
        "14 21 17 24  4";
        "10 16 15  9 19";
        "18  8 23 26 20";
        "22 11 13  6  5";
        " 2  0 12  3  7";
      ]
    = 0
end

let run () =
  let input = "./lib/day_4/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 4A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 4B: %i\n%!" in
  ()
