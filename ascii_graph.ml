exception Small_Data_Set

type filename = string

exception CSV_Parse_Failure

let csv_to_lst (file : filename) =
  let input_stream = open_in file in
  let rec scan l =
    match
      try Some (input_line input_stream)
      with End_of_file ->
        let close un = Stdlib.close_in input_stream in
        close ();
        None
    with
    | None -> List.rev l
    | Some h -> (
        try
          let vals = String.split_on_char ',' h in

          let p_at_time = Float.of_string (List.nth vals 1) in
          scan (p_at_time :: l)
        with ex -> scan l)
  in
  scan []

let rec avg_cut_list (l : float list) (size : int) =
  let rec avg_cut orig_list nlist =
    match orig_list with
    | [] -> List.rev nlist
    | e1 :: e2 :: t ->
        let avg = (e1 +. e1) /. 2. in
        avg_cut t (avg :: nlist)
    | h :: t -> avg_cut t (h :: nlist)
  in
  if List.length l > size then avg_cut_list (avg_cut l []) size else l

let heights (l : float list) =
  let vals = avg_cut_list l 200 in
  List.map
    (fun (e : float) ->
      (e -. 2000.) /. 100. |> Float.floor |> int_of_float)
    vals

let rec print_ascii graph s =
  match graph with
  | [] -> s
  | h :: t -> (
      match h with
      | true -> print_ascii t (s ^ "*")
      | false -> print_ascii t (s ^ " "))

let graph_ascii (l : float list) (s : string) =
  let col_height = heights l in
  let rec graph info i max sr =
    let l_to_p = List.map (fun e -> e >= i) info in
    if i > max then sr
    else graph info (i + 1) max (print_ascii l_to_p "" ^ "\n" ^ sr)
  in
  graph col_height 0 10 ""

let string_graph filename = graph_ascii (csv_to_lst filename) ""

let make_graph filename = print_string (string_graph filename)
