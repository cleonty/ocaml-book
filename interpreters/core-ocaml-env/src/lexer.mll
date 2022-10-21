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
  | "," { COMMA }
  | "->" { ARROW }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "fst" { FIRST}
  | "snd" { SECOND }
  | "<=" { LEQ }
  | "<" { LE }
  | ">=" { GEQ }
  | ">" { GE }
  | "=" { EQUALS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "Left" { LEFT }
  | "Right" { RIGHT }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { ALT }
  | "|" { ARROW }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }