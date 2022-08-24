
{
  open Parser;;
  open Printf
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "Float"	{ FLOAT }
  | "*10"	{ MULT10 }
  | "/10"	{ DIV10 }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | '{'		{ BRACKET }
  | '}'		{ BRACKET2 }
  | '['   { SQBRACKET }
  | ']'   { SQBRACKET2 }
  | ','		{ COMA }
  | '@'   { AMPERSAN }
  | '"'   { LSTRING (stringRULE [] lexbuf) }
  | "->"        { ARROW }
  | ".one"	{ ONE }
  | ".two"	{ TWO }
  | "*"		{ ASTERISK }
  | "."['0'-'9']+
		{ FLOATV (String.sub(Lexing.lexeme lexbuf)(1)(String.length(Lexing.lexeme lexbuf)-1)) }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

and stringRULE buf = parse
  | '"'                   { buf }
  | ['a'-'z'] as charV
    { let b = charV::buf in
      stringRULE b lexbuf
    }
  | _                     {raise Lexical_error}
  (*| eof                   { EOF } *)


