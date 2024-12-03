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

let per_line_operation line =
    Printf.printf "Line is: %s\n" line
