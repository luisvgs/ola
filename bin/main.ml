open Parser

let () =
  let input = Op.LazyStream.of_string "3+1" in
    match Parser.Op.parse expr input with
        | Some ans -> Printf.printf "%d\n" ans
        | None -> print_endline "ERROR!"
