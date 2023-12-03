open Core
open In_channel

let file = "data/day2.full"
let contains s1 s2 = String.is_substring ~substring:s2 s1

let split_clear str char =
  String.split str ~on:char |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

let max_value = function
  | "red" -> 12
  | "green" -> 13
  | "blue" -> 14
  | _ -> raise (Invalid_argument "empty string")
;;

let check_ball ball = Int.of_string ball.(0) <= max_value ball.(1)

let check list =
  let balls = split_clear list ',' in
  List.map balls ~f:(fun s -> split_clear s ' ' |> Array.of_list |> check_ball)
  |> List.reduce_exn ~f:( && )
;;

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
;;

let solve () =
  let ic = create file in
  process_lines ic 0 |> Int.to_string |> print_endline
;;
