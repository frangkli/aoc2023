open Core
open In_channel

let file = "data/day2.full"

let red_max, green_max, blue_max = (12, 13, 14)

let contains s1 s2 = String.is_substring ~substring:s2 s1

let max_value = function
  | "red" -> red_max
  | "green" -> green_max
  | "blue" -> blue_max
  | _ -> raise (Invalid_argument "empty string")

let colorize list =
  List.exists ~f:(fun x -> contains x "red") list
  && Int.of_string (List.nth_exn list 0) <= red_max
  || List.exists ~f:(fun x -> contains x "green") list
     && Int.of_string (List.nth_exn list 0) <= green_max
  || List.exists ~f:(fun x -> contains x "blue") list
     && Int.of_string (List.nth_exn list 0) <= blue_max

let check list =
  let balls =
    String.split list ~on:','
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  List.map balls ~f:(fun s ->
      String.split s ~on:' '
      |> List.filter ~f:(fun s -> not (String.is_empty s))
      |> colorize)
  |> List.reduce_exn ~f:( && )

let rec process_lines ic score =
  let line = input_line ic in
  match line with
  | None -> score
  | Some line ->
    let arr = String.split line ~on:':' in
    let game_num =
      Int.of_string (List.nth_exn (String.split (List.nth_exn arr 0) ~on:' ') 1)
    in
    let takes = List.nth_exn arr 1 in
    let game = String.split takes ~on:';' in
    let ok = List.map game ~f:check |> List.reduce_exn ~f:( && ) in
    let res = if ok then game_num else 0 in
    process_lines ic (res + score)

let solve () =
  let ic = create file in
  process_lines ic 0 |> Int.to_string |> print_endline
