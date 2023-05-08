open Opal
module Op = Opal;;

let parens = between (exactly '(') (exactly ')')
let integer = many1 digit => implode % int_of_string
let bool = token "true" <|> token "false"
let add = exactly '+' >> return ( + )
let sub = exactly '-' >> return ( - )
let mul = exactly '*' >> return ( * )
let div = exactly '/' >> return ( / )

let rec expr input = chainl1 term (add <|> sub) input
and term input = chainl1 factor (mul <|> div) input
and factor input = (parens expr <|> integer) input
