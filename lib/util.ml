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
