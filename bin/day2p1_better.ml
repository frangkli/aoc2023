open Core
open In_channel

let file = "data/day2.full"
let contains s1 s2 = String.is_substring ~substring:s2 s1

let clean_split ?(on = ' ') str =
  String.split str ~on
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:String.strip
;;

let max_value = function
  | "red" -> 12
  | "green" -> 13
  | "blue" -> 14
  | _ -> raise (Invalid_argument "empty string")
;;

let check entry = Int.of_string entry.(0) <= max_value entry.(1)

let check list =
  let entries = clean_split list ~on:',' in
  List.map entries ~f:(fun s -> clean_split s |> Array.of_list |> check)
  |> List.reduce_exn ~f:( && )
;;

let rec process_lines ic score =
  let line = input_line ic in
  match line with
  | None -> score
  | Some line ->
    let arr = clean_split line ~on:':' |> Array.of_list in
    let game_num = Int.of_string (List.nth_exn (clean_split arr.(0)) 1) in
    let game = clean_split arr.(1) ~on:';' in
    let ok = List.map game ~f:check |> List.reduce_exn ~f:( && ) in
    let res = if ok then game_num else 0 in
    process_lines ic (score + res)
;;

let solve () =
  let ic = create file in
  process_lines ic 0 |> Int.to_string |> print_endline
;;
