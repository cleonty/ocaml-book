{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let digit = ['0'-'9']
let int = '-'? digit+

rule read = 
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | "fun" { FUN }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }