let read_file filename =
  let chan = open_in filename in
  let try_read () = try Some (input_line chan) with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some s -> loop (s :: lines)
    | None ->
        close_in chan;
        List.rev lines
  in
  loop []

let parse fmt map line = try Some (Scanf.sscanf line fmt map) with _ -> None

let rec try_parse parsers line =
  match parsers with
  | [] -> failwith ("could not parse: " ^ line)
  | parse :: parsers -> (
      match parse line with
      | None -> try_parse parsers line
      | Some result -> result
    )

let char_list_of_string s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter ~f:(Buffer.add_char buf) chars;
  Buffer.contents buf

let cons_uniq tl hd = if List.mem hd ~set:tl then tl else hd :: tl

let remove_duplicates xs = List.rev (List.fold_left ~f:cons_uniq ~init:[] xs)

let range a b =
  let rec aux a b = if a > b then [] else a :: aux (a + 1) b in
  if a > b then List.rev (aux b a) else aux a b
