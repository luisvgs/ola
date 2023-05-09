open Opal
module Op = Opal;;


type exp = PlusExp of exp * exp
    | SubExp of exp * exp
    | MulExp of exp * exp
    | DivExp of exp * exp
    | Variable of string
    | Number of int
    | LTExp of exp * exp
    | GTExp of exp * exp
    | AndExp of exp * exp
    | OrExp of exp * exp
    | Bool of bool
    [@@deriving show];;


let reserved = [
    "true";
    "false";
    "if";
    "then";
    "else";
    "while";
    "do";
    "and";
    "or";
]

let ident = (spaces >> letter <~> many alpha_num) => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

let number = spaces >> many1 digit => implode % int_of_string
let parens = between (token "(") (token ")")
let addop = token "+" >> return (fun x y -> PlusExp(x, y))
let subop = token "-" >> return (fun x y -> SubExp(x, y))
let mulop = token "*" >> return (fun x y -> MulExp(x, y))
let divop = token "/" >> return (fun x y -> DivExp(x, y))
let ltop  = token "<" >> return (fun x y -> LTExp(x, y))
let gtop  = token ">" >> return (fun x y -> GTExp(x, y))
let orop  = token "or"  >> return (fun x y -> OrExp(x, y))
let andop = token "and" >> return (fun x y -> AndExp(x, y))
let atom = (ident => (fun s -> Variable s))
       <|> (number => (fun x -> Number x))
       <|> (token "true" >> return (Bool true))
       <|> (token "false" >> return (Bool false))

let rec expr input = (chainl1 and_expr orop) input
and and_expr input = (chainl1 rop_expr andop) input
and rop_expr input = (chainl1 add_expr (ltop <|> gtop)) input
and add_expr input = (chainl1 mul_expr (addop <|> subop)) input
and mul_expr input = (chainl1 prm_expr (mulop <|> divop)) input
and prm_expr input = (parens expr <|> atom) input
