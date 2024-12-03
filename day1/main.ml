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
    let split = Str.split (Str.regexp_string "   ") line in
    let num1 = int_of_string (List.nth split 0);
    in
    let num2 = int_of_string (List.nth split 1);
    in
    (num1, num2)


let part1() =
    let lines = read_lines_from_files file in
    let nums = List.map return_nums lines in
    let nums1, nums2 = List.split nums in
    let sorted_nums1 = List.sort compare nums1 in
    let sorted_nums2 = List.sort compare nums2 in

    let diff = List.map2 abs_diff sorted_nums1 sorted_nums2 in
    let total_diff = sum diff in
    Printf.printf "Result part 1: %d\n" total_diff
    ;;

let similarity nums1 nums2 =
    let value_count = List.map
        (fun x ->
            let count = List.fold_left(fun acc target ->
                if x = target then acc + 1 else acc) 0 nums2 in
            (x, count)
            ) nums1
        in
        List.fold_left (fun acc (x, y) -> acc + (x*y)) 0 value_count
    ;;

let part2() =
    let lines = read_lines_from_files file in
    let nums = List.map return_nums lines in
    let nums1, nums2 = List.split nums in
    let result = similarity nums1 nums2 in
    Printf.printf "Result part 2: %d\n" result
;;

part1();
part2();
