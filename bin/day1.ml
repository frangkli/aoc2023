open Core
open In_channel

let file = "data/day1.full"
let digits = [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]
let words = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let to_int (str : string) =
  let i = List.findi digits ~f:(fun _ x -> String.equal x str) in
  match i with
  | Some (index, _) -> index + 1
  | None ->
    let j = List.findi words ~f:(fun _ x -> String.equal x str) in
    (match j with
     | Some (index, _) -> index + 1
     | None -> 0)
;;

let most line str i comp =
  let indices = String.substr_index_all line ~may_overlap:false ~pattern:str in
  match indices with
  | [] -> i
  | _ ->
    let index = List.fold_left indices ~init:(List.nth_exn indices 0) ~f:comp in
    (match index with
     | -1 -> i
     | _ -> comp i index)
;;

let rec find_most line arr (value, i) comp =
  match arr with
  | [] -> value
  | hd :: tl ->
    let index = most line hd i comp in
    if index = i
    then find_most line tl (value, i) comp
    else find_most line tl (to_int hd, index) comp
;;

let find_left_most line arr = find_most line arr (0, String.length line) min
let find_right_most line arr = find_most line arr (0, -1) max

let get_score line arr =
  let left = find_left_most line arr in
  let right = find_right_most line arr in
  (left * 10) + right
;;

let rec part_1 ic total =
  let line = input_line ic in
  match line with
  | None -> total
  | Some line -> get_score line digits |> ( + ) total |> part_1 ic
;;

let rec part_2 ic total =
  let line = input_line ic in
  match line with
  | None -> total
  | Some line -> get_score line (digits @ words) |> ( + ) total |> part_2 ic
;;

let solve () =
  let ic1 = create file in
  let ic2 = create file in
  part_1 ic1 0 |> Int.to_string |> print_endline;
  part_2 ic2 0 |> Int.to_string |> print_endline
;;
