{ 
module ProjectGrammar where 
import ProjectLexer 
}

%name projectParser
%tokentype { Token } 
%error { parseError } 
%token 
    BOOL   { TokenTypeBool _ } 
    INT    { TokenTypeInt _ }
    STRING { TokenTypeString _ }
    ARRAY  { TokenTypeArray _ } 
    int    { TokenInt _ $$ }
    string { TokenString _ $$ }
    '{'    { TokenStartFile _ }
    '}'    { TokenEndFile _ }
    IN     { TokenInput _ }
    '+'    { TokenPlus _ }
    '-'    { TokenSubtract _ }
    '*'    { TokenMultiply _ }
    '/'    { TokenDivide _ }
    '('    { TokenLParen _ }
    ')'    { TokenRParen _ }
    '<'    { TokenLessThan _ }
    '>'    { TokenGreaterThan _ }
    '='    { TokenEq _ }
    '!='   { TokenNotEq _ }
    '<='   { TokenLessOrEq _ }
    '>='   { TokenGreaterOrEq _ }
    '=*'   { TokenAssignment _ }
    intArray { TokenIntArray _ $$ }
    TRUE   { TokenTrue _ }
    FALSE  { TokenFalse _ }
    IF     { TokenIf _ }
    ENDIF  { TokenEndIf _ }
    ELSE   { TokenElse _ }
    WHILE  { TokenWhile _ }
    FOR    { TokenFor _ }
    ENDLOOP { TokenEndLoop _ }
    ':'    { TokenHasType _ }
    RETURN { TokenReturn _ }
    AND    { TokenAnd _ }
    OR     { TokenOr _ }
    XOR    { TokenXor _ }
    NOT    { TokenNot _ }
    AT     { TokenAt _ }
    ADD    { TokenAddToArr _ }
    var    { TokenVar _ $$ }
    LENGTH { TokenLength _ }
    ';'    { TokenEndStmt _ }



%nonassoc ';'
%nonassoc BOOL INT STRING ARRAY
%nonassoc IF WHILE FOR ELSE ENDIF ENDLOOP
%nonassoc '=*' ADD RETURN
%left AT
%left AND OR
%nonassoc XOR
%right NOT
%nonassoc '<' '>' '<=' '>=' '!=' '='
%left '+' '-'
%right '*' '/'
%left neg
%nonassoc LENGTH
%nonassoc var intArray IN
%nonassoc int string TRUE FALSE
%nonassoc '(' ')'
%nonassoc '{' '}'

%% 
Program : '{' Actions '}'            { $2 }

Actions : Actions Action         { $2 : $1 }
    | Action                         { [$1] }

Action : Begin ';'                     { $1 }

Begin : IF '(' Exp ')' Actions ENDIF              { PIf $3 $5 }
      | IF '(' Exp ')' Actions ELSE Actions ENDIF { PIfElse $3 $5 $7 }
      | WHILE '(' Exp ')' Actions ENDLOOP         { PWhile $3 (reverse ($5)) }
      | FOR '(' Exp ')' Actions ENDLOOP           { PFor $3 $5 }
      | RETURN Exp                                { PReturn $2 }
      | Type ':' var '=*' Exp                     { PAssignVar $1 (PVar $3) $5 }
      | var '=*' Exp                              { PUpdateVar (PVar $1) $3 }
      | var '+' '+'                               { PUpdateVar (PVar $1) (PAdd (PVar $1) (PInt 1))}
      | '(' Begin ')'                             { $2 }
      | ADD var Exp                               { PAddToIntArray (PVar $2) $3 }

Exp : int                                       { PInt $1 }
    | string                                    { PString $1 }
    | intArray                                  { PIntArray $1 }
    | var                                       { PVar $1 }
    | '-' Exp %prec neg                         { PNegative $2}
    | TRUE                                      { PTrue }
    | FALSE                                     { PFalse } 
    | Exp '*' Exp                               { PMult $1 $3 }
    | Exp '/' Exp                               { PDiv $1 $3 }
    | Exp '+' Exp                               { PAdd $1 $3 }
    | Exp '-' Exp                               { PSub $1 $3 }
    | Exp '<' Exp                               { PCompareLessMore $1 $3 }
    | Exp '>' Exp                               { PCompareMoreLess $1 $3 }
    | Exp '<=' Exp                              { PCompareEqLessMore $1 $3 }
    | Exp '>=' Exp                              { PCompareEqMoreLess $1 $3 }
    | Exp '!=' Exp                              { PNotEqual $1 $3 }
    | Exp '=' Exp                               { PEqual $1 $3 }
    | NOT Exp                                   { PNot $2 }
    | Exp XOR Exp                               { PXor $1 $3 }
    | Exp AND Exp                               { PAnd $1 $3 }
    | Exp OR Exp                                { POr $1 $3 }
    | var AT Exp                                { PGetIntValueAt (PVar $1) $3 }
    | IN AT Exp                                 { PGetInputValueAt $3 }
    | LENGTH var                                { PLength (PVar $2) }
    | '(' Exp ')'                               { $2 }


Type : BOOL                     { PBOOL } 
     | INT                      { PINT } 
     | STRING                   { PSTRING } 
     | ARRAY                    { PARRAY } 


{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Type = PINT | PBOOL | PSTRING | PARRAY
   deriving (Show,Eq)

type Environment = [ (String,Expr) ]

data Expr = PInt Int | PString String | PIntArray [Int] | PVar String | PNegative Expr | PIn [[Int]] 
            | PTrue | PFalse 
            | PMult Expr Expr | PDiv Expr Expr | PAdd Expr Expr | PSub Expr Expr 
            | PCompareLessMore Expr Expr | PCompareMoreLess Expr Expr | PCompareEqLessMore Expr Expr | PCompareEqMoreLess Expr Expr 
            | PNotEqual Expr Expr | PEqual Expr Expr | PNot Expr | PXor Expr Expr | PAnd Expr Expr | POr Expr Expr
            | PIf Expr [Expr] | PIfElse Expr [Expr] [Expr] | PWhile Expr [Expr] | PFor Expr [Expr] 
            | PReturn Expr | PAssignVar Type Expr Expr | PUpdateVar Expr Expr | PGetIntValueAt Expr Expr | PAddToIntArray Expr Expr
            | PGetInputValueAt Expr | PLength Expr
    deriving (Show,Eq)
} 