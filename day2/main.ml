#use "topfind";;
#require "str";;

let file = "input_1.txt"

let abs_diff x y = abs (x - y)

let rec sum nums =
    match nums with
    | [] -> 0
    | x::xs -> x + sum xs
;;

let rec read_lines_from_files file =
    let ic = open_in file in
    let rec read_lines str_list =
        try
            let line = input_line ic in
            read_lines(line :: str_list)
        with
            End_of_file ->
                close_in ic;
                List.rev str_list
        in
    read_lines []

let return_nums line =
    let split = Str.split (Str.regexp_string " ") line in
    let values = List.map (fun x -> int_of_string x) split in
    values

let rec is_safe init prev nums gradient =
    match nums with
    | [] -> 1
    | x::xs when init=0 ->
        is_safe 1 x xs 0
    | x::xs ->
            let diff = (x -  prev) in
            if abs(diff) < 1 then 0
            else if abs(diff) > 3 then 0
            else if gradient <> 0 && ((diff * gradient) < 0) then 0
            else is_safe 1 x xs (if gradient = 0 then diff else gradient)
;;

let part1() =
    let lines = read_lines_from_files file in
    let nums = List.map return_nums lines in
    let safe = List.map (fun x -> is_safe 0 0 x 0) nums in
    Printf.printf "Result part 1: %d\n" (sum safe)
;;


let rec is_safe_2 init prev nums gradient fault =
    match nums with
    | [] -> 1
    | x::xs when init=0 ->
        is_safe_2 1 x xs 0 0
    | x::xs ->
            let diff = (x -  prev) in
            if abs(diff) < 1 then (if fault = 1 then 0 else is_safe_2 1 prev xs gradient 1)
            else if abs(diff) > 3 then (if fault = 1 then 0 else is_safe_2 1 prev xs gradient 1)
            else if gradient <> 0 && ((diff * gradient) < 0) then (if fault = 1 then 0 else is_safe_2 1 prev xs gradient 1)
            else is_safe_2 1 x xs (if gradient = 0 then diff else gradient) 0
;;

let rec remove_i i nums =
    match nums with
    | [] -> []
    | x::xs -> if i == 0 then xs else x::remove_i (i - 1) xs
;;

let rec is_safe_with_remove nums =
    if (is_safe 0 0 nums 0 = 1) then 1
    else
        let length = List.length nums in
        let rec check_removal idx =
            if idx >= length then 0
            else
                let new_nums = remove_i idx nums in
                if is_safe 0 0 new_nums 0 = 1 then 1 else check_removal (idx+1)
        in
        check_removal 0
;;


let part2() =
    let lines = read_lines_from_files file in
    let nums = List.map return_nums lines in
    (* A bit of brute force, would have preffered to get safe 2 working *)
    let safe = List.map is_safe_with_remove nums in
    Printf.printf "Result part 2: %d\n" (sum safe);
;;

part1();
part2();
