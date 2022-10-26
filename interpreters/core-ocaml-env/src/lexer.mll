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
  | "/" { DIV }
  | "+" { PLUS }
  | "-" { MINUS }
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
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }