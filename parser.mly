
%{
  open Lambda;;
  open Printf;;
  open List;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token NAT
%token FLOAT
%token MULT10
%token DIV10

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token BRACKET
%token BRACKET2
%token SQBRACKET
%token SQBRACKET2
%token AMPERSAN
%token COMA
%token ASTERISK
%token ONE
%token TWO

%token <char list> LSTRING
%token <int> INTV
%token <string> STRINGV
%token <string> FLOATV

%start s
%type <Lambda.term> s

%%
s :
    term EOF { $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLet ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLet($2, TmFix(TmAbs($2, $4, $6)), $8) }
  | STRINGV EQ term
      {TmEq ($1,$3) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | MULT10 atomicTerm
      {TmFloatDown($2)}
  | DIV10 atomicTerm
      {TmFloat($2)}

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | INTV FLOATV
    { let x = int_of_string(  String.concat("")([string_of_int($1);$2])  ) in
	let rec ff term = function
		0 -> term
		| i -> ff(TmFloat(term))(i-1) in


	let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in ff(f x)(String.length(  $2  ))}
  | term ONE
     {TmProj1($1)}
  | term TWO
     {TmProj2($1)}
  | term SQBRACKET INTV SQBRACKET2
     {TmProj($1,$3)}
  | term DOT STRINGV
     {TmProjRCD($1,$3)}
  | pair
     {$1}
  | tuple
     {$1}
  | str
     {TmString($1)}
  | record
     {$1}

record:
    AMPERSAN BRACKET BRACKET2
      {TmRecord([])}
  | AMPERSAN BRACKET record_elements BRACKET2
      {TmRecord($3)}

record_elements:
    record_item
        {$1}
  | record_item COMA record_elements
        {$1@$3}

record_item:
    STRINGV COLON EQ term
      {[($1,$4)]}   

str:
    LSTRING
    { TmTuple(let convert = fun x -> TmChar(x) in List.map convert (List.rev $1) ) }

tuple:
  BRACKET BRACKET2
    {TmTuple([])}
  | BRACKET tuple_element BRACKET2
    {TmTuple($2)}
tuple_element:
  term
    {[$1]}
  | term COMA tuple_element
    {[$1]@$3}

pair:
  BRACKET term COMA term BRACKET2
      {TmPair($2,$4)}

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | atomicTy ASTERISK atomicTy
      {TyPair($1,$3)}
  | BRACKET tupleTy BRACKET2
      {TyTuple($2)}
  | AMPERSAN BRACKET recordTy BRACKET2
      {TyRecord($3)}

tupleTy:
  atomicTy
     {[$1]}
  | atomicTy ASTERISK tupleTy
     {[$1]@$3}

recordTy:
  STRINGV atomicTy
     {[($1, $2)]}
  | STRINGV atomicTy ASTERISK recordTy
     {[($1,$2)]@$4}

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | FLOAT
      { TyFloat }
