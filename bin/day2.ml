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

let rec part_1 ic sum =
  let line = input_line ic in
  match line with
  | None -> sum
  | Some line ->
    let arr = clean_split line ~on:':' |> Array.of_list in
    let game_num = Int.of_string (List.nth_exn (clean_split arr.(0)) 1) in
    let game = clean_split arr.(1) ~on:';' in
    let ok = List.map game ~f:check |> List.reduce_exn ~f:( && ) in
    let res = if ok then game_num else 0 in
    part_1 ic (sum + res)
;;

let get_number str color =
  match List.find str ~f:(fun s -> contains s color) with
  | None -> 0
  | Some s -> Int.of_string (List.nth_exn (clean_split s) 0)
;;

let order_entry entry =
  let red = get_number entry "red" in
  let green = get_number entry "green" in
  let blue = get_number entry "blue" in
  red, green, blue
;;

let rec find_max entry (red, green, blue) =
  match entry with
  | [] -> red, green, blue
  | h :: t ->
    let red_, green_, blue_ = clean_split h ~on:',' |> order_entry in
    find_max t (max red red_, max green green_, max blue blue_)
;;

let rec part_2 ic power =
  let line = input_line ic in
  match line with
  | None -> power
  | Some line ->
    let arr = clean_split line ~on:':' |> Array.of_list in
    let game = clean_split arr.(1) ~on:';' in
    let red, green, blue = find_max game (0, 0, 0) in
    part_2 ic (power + (red * green * blue))
;;

let solve () =
  let ic1 = create file in
  let ic2 = create file in
  part_1 ic1 0 |> Int.to_string |> print_endline;
  part_2 ic2 0 |> Int.to_string |> print_endline
;;
