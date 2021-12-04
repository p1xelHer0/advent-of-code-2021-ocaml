let bit_of_char c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | _ -> failwith (String.make 1 c ^ ": not a bit")

let invert_bit bit =
  match bit with
  | 0 -> 1
  | 1 -> 0
  | _ -> failwith (string_of_int bit ^ ": not a bit")

let col_of_input input i = List.map ~f:(fun a -> a.(i)) input

let dec_of_bits bits =
  snd
  @@ List.fold_right
       ~f:(fun b (pow, acc) -> (2 * pow, if b > 0 then acc + pow else acc))
       ~init:(1, 0) bits

let bits_of_string s =
  Util.char_list_of_string s |> List.map ~f:bit_of_char |> Array.of_list

module A = struct
  open Aoc_2021

  let most_common_bit bits =
    let sum_of_bits = List.fold_left ~f:( + ) ~init:0 bits in
    if sum_of_bits * 2 > List.length bits then 1 else 0

  let solve l =
    let bits = List.map ~f:bits_of_string l in

    let gamma =
      Util.range 0 (Array.length (List.hd bits) - 1)
      |> List.map ~f:(col_of_input bits)
      |> List.map ~f:most_common_bit
    in
    let epsilon = List.map ~f:invert_bit gamma in
    dec_of_bits gamma * dec_of_bits epsilon

  let%test _ =
    solve
      [
        "00100";
        "11110";
        "10110";
        "10111";
        "10101";
        "01111";
        "00111";
        "11100";
        "10000";
        "11001";
        "00010";
        "01010";
      ]
    = 198
end

module B = struct
  let most_common_bit bits =
    let sum_of_bits = List.fold_left ~f:( + ) ~init:0 bits in
    let length = List.length bits in
    if sum_of_bits * 2 >= length then 1 else 0

  let solve l =
    let bits = List.map ~f:bits_of_string l in

    let rec solve_aux ?(least_common = false) input i =
      let f = if least_common then invert_bit else fun a -> a in

      match input with
      | [ x ] -> Array.to_list x |> dec_of_bits
      | _ ->
          let bit_to_keep = col_of_input input i |> most_common_bit |> f in
          solve_aux ~least_common
            (List.filter ~f:(fun a -> a.(i) = bit_to_keep) input)
            (i + 1)
    in
    let oxygen = solve_aux bits 0 in
    let co2 = solve_aux ~least_common:true bits 0 in
    oxygen * co2

  let%test _ =
    solve
      [
        "00100";
        "11110";
        "10110";
        "10111";
        "10101";
        "01111";
        "00111";
        "11100";
        "10000";
        "11001";
        "00010";
        "01010";
      ]
    = 230
end

let run () =
  let input = "./lib/day_3/input" in

  let parsed_input = input |> Util.read_file in

  let _puzzle_a = parsed_input |> A.solve |> Printf.printf "Day 3A: %i\n%!" in

  let _puzzle_b = parsed_input |> B.solve |> Printf.printf "Day 3B: %i\n%!" in
  ()
