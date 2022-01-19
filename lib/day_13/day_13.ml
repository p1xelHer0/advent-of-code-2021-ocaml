open ContainersLabels

type fold_type =
  | X of int
  | Y of int

type instruction =
  | Coordinate of (int * int)
  | Fold of fold_type

let parsers =
  Aoc_2021.Util.
    [
      parse "%i,%i" (fun x y -> Coordinate (x, y));
      parse "fold along x=%i" (fun x -> Fold (X x));
      parse "fold along y=%i" (fun y -> Fold (Y y));
    ]
  

let parse_instructions = Aoc_2021.Util.try_parse parsers

let print_matrix matrix =
  Array.iter
    ~f:(fun a ->
      Array.iter ~f:print_char a;
      print_newline ()
    )
    matrix;
  print_newline ()

let matrix_w coordinates =
  coordinates
  |> List.sort ~cmp:(fun (x1, _) (x2, _) -> Int.compare x2 x1)
  |> List.hd
  |> fun (x, _) -> x |> succ

let matrix_h coordinates =
  coordinates
  |> List.sort ~cmp:(fun (_, y1) (_, y2) -> Int.compare y2 y1)
  |> List.hd
  |> fun (_, y) -> y |> succ

let matrix_from_coordinates matrix =
  Array.make_matrix ~dimx:(matrix_h matrix) ~dimy:(matrix_w matrix) ' '

let rec fold_x coordinates ~x ~init:accu =
  let c =
    match coordinates with
    | [] -> accu
    | (x', y') :: tl ->
        if x < x'
        then
          let new_x = x - (x' - x) in
          fold_x ~x ~init:((new_x, y') :: accu) tl
        else fold_x ~x ~init:accu tl
  in
  c |> List.filter ~f:(fun (x', _) -> x > x')

let rec fold_y coordinates ~y ~init =
  let c =
    match coordinates with
    | [] -> init
    | (x', y') :: tl ->
        if y < y'
        then
          let new_y = y - (y' - y) in
          fold_y ~y ~init:((x', new_y) :: init) tl
        else fold_y ~y ~init tl
  in
  c |> List.filter ~f:(fun (_, y') -> y > y')

let pick_fold_type ~coordinates = function
  | X x -> fold_x ~init:coordinates ~x coordinates
  | Y y -> fold_y ~init:coordinates ~y coordinates

let coordinates_of_instructions instructions =
  List.filter_map
    ~f:(function Fold _ -> None | Coordinate c -> Some c)
    instructions

let folds_of_instructions instructions =
  List.filter_map
    ~f:(function Coordinate _ -> None | Fold f -> Some f)
    instructions

module A = struct
  let solve l =
    let instructions = l |> List.map ~f:parse_instructions in
    let random_dots = coordinates_of_instructions instructions in
    let folds = folds_of_instructions instructions in

    let first_instruction = List.hd folds in

    let new_coordinates =
      pick_fold_type ~coordinates:random_dots first_instruction
    in
    Aoc_2021.Util.remove_duplicates new_coordinates |> List.length

  let%test _ =
    solve
      [
        "6,10";
        "0,14";
        "9,10";
        "0,3";
        "10,4";
        "4,11";
        "6,0";
        "6,12";
        "4,1";
        "0,13";
        "10,12";
        "3,4";
        "3,0";
        "8,4";
        "1,10";
        "2,14";
        "8,10";
        "9,0";
        "fold along y=7";
        "fold along x=5";
      ]
    = 17
end

module B = struct
  let solve l =
    let instructions = l |> List.map ~f:parse_instructions in
    let random_dots = coordinates_of_instructions instructions in
    let folds = folds_of_instructions instructions in

    let new_dots =
      List.fold_left
        ~f:(fun accu f -> pick_fold_type ~coordinates:accu f)
        ~init:random_dots folds
      |> Aoc_2021.Util.remove_duplicates
    in

    let new_paper = matrix_from_coordinates new_dots in

    List.iter ~f:(fun (x, y) -> new_paper.(y).(x) <- '@') new_dots;
    print_endline "Day 13B:";
    print_matrix new_paper
end

let run () =
  let input = "./lib/day_13/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 13A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve in
  ()
