module ProjectTypes where
import ProjectGrammar

type VarTypes = [(String, Type)]

--this could be a hashmap to be more efficient
--gets the type of a stored variable
getVarType :: String -> VarTypes -> Type
getVarType a [] = error ("Variable '" ++ a ++ "' doesn't exist")
getVarType a ((b,c):xs) | a == b = c
                        | otherwise = getVarType a xs

--adds a new variable to the list of variables
addVarType :: String -> Type -> VarTypes -> VarTypes
addVarType a b xs | (any (\(x,_) -> x == a) xs) == True = error ("Variable re-declaration occured for var '" ++ a ++ "'")
                  | otherwise = xs ++ [(a,b)]

--iterates through the list of actions and evaluates its expression
--it needs to do loops and conditions separately as they include a list of actions to do in their body
typeChecks :: VarTypes -> [Expr] -> [(VarTypes,Type)]
typeChecks xs ((PIf a b):[]) | (snd ans) == PBOOL = bns
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs ((PIfElse a b c):[]) | (PBOOL) == (snd ans) = bns ++ cns
    where (ans,bns,cns) = (typeCheck xs a,typeChecks xs b,typeChecks (fst(last bns)) c)
typeChecks xs ((PWhile a b):[]) | (PBOOL) == (snd ans) = bns
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs ((PFor a b):[]) | (PBOOL) == (snd ans) = bns
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs (a:[]) = [typeCheck xs a]
typeChecks xs ((PIf a b):as) | (snd ans) == PBOOL = bns ++ typeChecks (fst (last bns)) as
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs ((PIfElse a b c):as) | (PBOOL) == (snd ans) = bns ++ cns ++ typeChecks (fst(last cns)) as
    where (ans,bns,cns) = (typeCheck xs a,typeChecks xs b,typeChecks (fst(last bns)) c)
typeChecks xs ((PWhile a b):as) | (PBOOL) == (snd ans) = bns ++ typeChecks (fst (last bns)) as
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs ((PFor a b):as) | (PBOOL) == (snd ans) = bns ++ typeChecks (fst (last bns)) as
    where (ans,bns) = (typeCheck xs a,typeChecks xs b)
typeChecks xs (a:as) = ([output] ++ (typeChecks (fst output) as))
    where output = typeCheck xs a
typeChecks xs a = error ("Type error " ++ show a)

--checks the type of an expression by checking the type of its elements
typeCheck :: VarTypes -> Expr -> (VarTypes,Type)
typeCheck xs (PInt _) = (xs,(PINT))
typeCheck xs (PString _) = (xs,(PSTRING))
typeCheck xs (PTrue) = (xs,(PBOOL))
typeCheck xs (PFalse) = (xs,(PBOOL))
typeCheck xs (PIntArray _) = (xs,(PARRAY))
typeCheck xs (PNegative _) = (xs,(PINT))
typeCheck xs (PVar a) = (xs,(getVarType a xs))
typeCheck xs (PMult a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PINT))
typeCheck xs (PDiv a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PINT))
typeCheck xs (PAdd a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PINT))
typeCheck xs (PSub a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PINT))
typeCheck xs (PCompareLessMore a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs a)) = (xs,(PBOOL))
typeCheck xs (PCompareMoreLess a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs a)) = (xs,(PBOOL))
typeCheck xs (PCompareEqLessMore a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs a)) = (xs,(PBOOL))
typeCheck xs (PCompareEqMoreLess a b) | ((PINT), (PINT)) == (snd(typeCheck xs a),snd(typeCheck xs a)) = (xs,(PBOOL))
typeCheck xs (PNotEqual _ _) = (xs,(PBOOL))
typeCheck xs (PEqual _ _) = (xs,(PBOOL))
typeCheck xs (PNot a) | snd(typeCheck xs a) == (PBOOL) = (xs,(PBOOL))
typeCheck xs (PXor a b) | ((PBOOL), (PBOOL)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PBOOL))
typeCheck xs (PAnd a b) | ((PBOOL), (PBOOL)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PBOOL))
typeCheck xs (POr a b) | ((PBOOL), (PBOOL)) == (snd(typeCheck xs a),snd(typeCheck xs b)) = (xs,(PBOOL))
typeCheck xs (PReturn a) | (snd ans) == (PINT) || (snd ans) == (PBOOL) || (snd ans) == (PSTRING) || (snd ans) == (PARRAY) = (xs, snd ans)
    where ans = typeCheck xs a
typeCheck xs (PAssignVar a (PVar b) c) | snd (typeCheck xs c) == a = (addVarType b a xs, a)
typeCheck xs (PUpdateVar b c) | (ans) == (typeCheck xs c) = ans
    where ans = (typeCheck xs b)
typeCheck xs (PGetIntValueAt b c) | ((snd (typeCheck xs b)) == (PARRAY)) && ((snd (typeCheck xs c)) == (PINT)) = (xs,(PINT))
typeCheck xs (PGetInputValueAt c) | (snd (typeCheck xs c)) == (PINT) = (xs,PARRAY)
typeCheck xs (PAddToIntArray b c) | ((snd (typeCheck xs b)) == (PARRAY)) && ((snd (typeCheck xs c)) == (PINT)) = (xs,(PARRAY))
typeCheck xs (PLength a) | snd (typeCheck xs a) == (PARRAY) = (xs,(PINT))
typeCheck xs a = error ("Type error " ++ show a)

--returns the type as a string
printType :: Type -> String
printType (PINT) = "Int"
printType (PBOOL) = "Bool"
printType (PSTRING) = "String"
printType (PARRAY) = "Array"

--if you wish, you may output the  list of the types of the expressions with this method
printTypes :: [(VarTypes,Type)] -> String
printTypes (x:[]) = printType (snd x)
printTypes (x:xs) =  (printType (snd x)) ++ "," ++ (printTypes xs)