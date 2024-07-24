{ 
module ProjectLexer where 
}

--posn wrapper so the row and collumn of the token can be printed in the event of an error
%wrapper "posn" 
$digit = 0-9      
@num = $digit+  
--numbers  
$alpha = [a-zA-Z] 
$lower = [a-z]   
--alphabetic characters   
--Variable Names
@string = \" .* \"      

tokens :-
$white+       ; 
  "--".*        ; 
  BOOL           { tok (\p s -> TokenTypeBool p) }
  INT            { tok (\p s -> TokenTypeInt p) }
  STRING         { tok (\p s -> TokenTypeString p) }
  ARRAY          { tok (\p s -> TokenTypeArray p) }
 -- \,             { tok (\p s -> TokenComma p) }
  @num           { tok (\p s -> TokenInt p (read s)) }
  @string        { tok (\p s -> TokenString p s) }
  "{"            { tok (\p s -> TokenStartFile p) }
  "}"            { tok (\p s -> TokenEndFile p) }
  IN             { tok (\p s -> TokenInput p) }
  \+             { tok (\p s -> TokenPlus p) }
  \-             { tok (\p s -> TokenSubtract p) }
  \*             { tok (\p s -> TokenMultiply p) }
  \/             { tok (\p s -> TokenDivide p) }
  "("            { tok (\p s -> TokenLParen p) }
  ")"            { tok (\p s -> TokenRParen p) }
  \<             { tok (\p s -> TokenLessThan p) }
  \>             { tok (\p s -> TokenGreaterThan p) }
  \=             { tok (\p s -> TokenEq p ) }
  "!="           { tok (\p s -> TokenNotEq p) }
  "<="           { tok (\p s -> TokenLessOrEq p) }
  ">="           { tok (\p s -> TokenGreaterOrEq p) }
  "=*"           { tok (\p s -> TokenAssignment p) }
  "[]"           { tok (\p s -> TokenIntArray p []) } 
  "[" @num (\, @num)* "]"  { tok (\p s -> TokenIntArray p (read s :: [Int])) }
  TRUE           { tok (\p s -> TokenTrue p) }
  FALSE          { tok (\p s -> TokenFalse p) }
  IF             { tok (\p s -> TokenIf p) }
  ENDIF          { tok (\p s -> TokenEndIf p) }
  ELSE           { tok (\p s -> TokenElse p) }
  WHILE          { tok (\p s -> TokenWhile p) }
  FOR            { tok (\p s -> TokenFor p) }  
  ENDLOOP        { tok (\p s -> TokenEndLoop p) }
  \:             { tok (\p s -> TokenHasType p) }
  RETURN         { tok (\p s -> TokenReturn p)}
  AND            { tok (\p s -> TokenAnd p) }
  OR             { tok (\p s -> TokenOr p) }
  XOR            { tok (\p s -> TokenXor p) }
  NOT            { tok (\p s -> TokenNot p) }
  AT             { tok (\p s -> TokenAt p) }
  LENGTH         { tok (\p s -> TokenLength p) }
  ADD            { tok (\p s -> TokenAddToArr p) }
  $lower [$alpha $digit \_ \']*      { tok (\p s -> TokenVar p s) }
  \;             { tok (\p s -> TokenEndStmt p) } 
  \n+            ;

{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenTypeBool AlexPosn         | 
  TokenTypeInt  AlexPosn         |
  TokenTypeString AlexPosn       |
  TokenTypeArray AlexPosn        |
 -- TokenComma AlexPosn            |
  TokenInt AlexPosn Int          | 
  TokenString AlexPosn String    |
  TokenStartFile AlexPosn        |
  TokenEndFile AlexPosn          |
  TokenInput AlexPosn            |
  TokenPlus AlexPosn             |
  TokenSubtract AlexPosn         |
  TokenMultiply AlexPosn         |
  TokenDivide AlexPosn           |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLessThan AlexPosn         |
  TokenGreaterThan AlexPosn      |
  TokenEq AlexPosn               |
  TokenNotEq AlexPosn            |
  TokenLessOrEq AlexPosn         |
  TokenGreaterOrEq AlexPosn      |
  TokenAssignment AlexPosn       |
  TokenIntArray AlexPosn [Int]   |
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenIf AlexPosn               |
  TokenEndIf AlexPosn            |
  TokenElse AlexPosn             |
  TokenWhile AlexPosn            |
  TokenFor AlexPosn              |
  TokenEndLoop AlexPosn          |
  TokenHasType AlexPosn          |
  TokenReturn AlexPosn           |
  TokenAnd AlexPosn              |
  TokenOr AlexPosn               |
  TokenXor AlexPosn              |
  TokenNot AlexPosn              |
  TokenAt AlexPosn               |
  TokenLength AlexPosn           |
  TokenAddToArr AlexPosn         |
  TokenVar AlexPosn String       |
  TokenEndStmt AlexPosn          
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeString (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeArray (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenString (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStartFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtract (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessOrEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterOrEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAssignment (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntArray (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndLoop (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenHasType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenXor (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddToArr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEndStmt (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}