open Parser
open OlaParser

let () =
  let input = Op.LazyStream.of_string "true" in
    match Parser.Op.parse expr input with
        | Some ans -> print_endline (OlaParser.show_exp ans)
        | None -> print_endline "ERROR!"
