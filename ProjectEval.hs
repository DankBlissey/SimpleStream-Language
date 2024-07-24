module ProjectEval where
import ProjectGrammar
import System.IO
import Data.List
import Control.Monad.State

type EvalState = StateT Environment IO

--old code for methods of evaluation that became redundant

--addInputsToLists :: [Int] -> [[Int]] -> [[Int]]
--addInputsToLists inputs listOfLists = zipWith (\list input -> list ++ [input]) listOfLists inputs

--readFile :: Environment -> IO(Environment)
--readFile xs = do    inputLine <- getLine
--                    let arr = splitOn " " inputLine 
--                    input <- arr
--might be used

--outPut :: Environment -> Expr -> IO Environment
--outPut vs x = do    let val = value vs x
--                    let updatedEnv = updateVariables vs "Return" val
--                    putStrLn ((show (getInt val)) ++ "\n")
--                    return updatedEnv

--answer :: Environment -> [Expr] -> [Int]
--answer vs xs = getArray (getVariable (evalProg vs xs) (PVar "Return"))

--printReturn :: [Int] -> String
--printReturn (x:[]) = show x
--printReturn (x:xs) = (show x) ++ "\n" ++ printReturn xs

--evalProg :: Environment -> [Expr] -> Environment
--evalProg vs (x:[]) = eval vs x
--evalProg vs (x:xs) = evalProg (eval vs x) xs

--printOutput :: Expr -> IO ()
--printOutput (PInt a) = putStrLn $ (show $ a)
--printOutput (PString a) = putStrLn $ a
--printOutput (PTrue) = putStrLn $ "True"
--printOutput (PFalse) = putStrLn $ "False"
--printOutput (PIntArray a) = putStrLn $ (show $ a)
--printOutput a = putStrLn $ (show $ a)



--evalProgs :: Environment -> [Expr] -> EvalState Environment
--evalProgs vs ((PReturn a):xs) = do  env <- vs
--                                    let val = value env a
--                                    updatedEnv <- updateVariables env "Return" val
--                                    putStrLn ((show (getInt val)) ++ "\n")
--                                    re <- evalProgs updatedEnv xs
--                                    return (re)
--evalProgs vs ((PReturn a):[]) = do  let val = value vs a
--                                    updatedEnv <- (updateVariables vs "Return" val)
--                                    putStrLn ((show (getInt val)) ++ "\n")
--                                    return (vs)
--evalProgs vs ((PIf a b):xs) | value vs a == PTrue = do  return (evalProgs (evalProgs vs b) xs)
--                            | otherwise = do    return (evalProgs vs xs)
--evalProgs vs ((PIf a b):[]) | value vs a == PTrue = do  return (evalProgs vs b)
--                            | otherwise = do    return (vs)
--evalProgs vs ((PIfElse a b c):xs) | value vs a == PTrue = do    return (evalProgs (evalProgs vs b) xs)
--                                  | otherwise = do  return (evalProgs (evalProgs vs c) xs)
--evalProgs vs ((PIfElse a b c):[]) | value vs a == PTrue = do    return (evalProgs vs b)
--                                  | otherwise = do  return (evalProgs vs c)
--evalProgs vs ((PWhile a b):xs) | value vs a == PTrue = do  return (evalProgs (evalProgs vs b) ((PWhile a b):xs))
--                               | otherwise = do return (evalProgs vs xs)
--evalProgs vs ((PWhile a b):[]) | value vs a == PTrue = do   return (evalProgs (evalProgs vs b) [(PWhile a b)])
--                               | otherwise = do return (vs)
--evalProgs vs ((PFor a b):xs)   | i <= 0 = do return (evalProgs vs xs)
--                               | otherwise = do  return (evalProgs (evalProgs vs b) ((PFor (PInt (i - 1)) b):xs))
--    where i = getInt (value vs a)
--evalProgs vs ((PFor a b):[])   | i <= 0 = do return (vs)
--                               | otherwise = do  return (evalProgs (evalProgs vs b) [(PFor (PInt (i - 1)) b)])
--    where i = getInt
--evalProgs vs (x:xs) = do    return (evalProgs (fmap eval (x vs)) xs)
--evalProgs vs (x:[]) = do    return (vs)

--eval (PAssignVar a b c) vs = updateVariables vs (getString b) (value vs c)
--eval (PUpdateVar b c) vs = updateVariables vs (getString b) (value vs c)
--eval vs (PIf a b) | value vs a == PTrue = evalProg vs b
--                  | otherwise = vs
--eval vs (PIfElse a b c) | value vs a == PTrue = evalProg vs b
--                        | otherwise = evalProg vs c
--eval vs (PWhile a b) | value vs a == PTrue = eval (evalProg vs b) (PWhile a b)
--                     | otherwise = vs
--eval vs (PFor a b) | i == 0 = vs
--                   | otherwise = eval (evalProg vs b) (PFor (PInt (i - 1)) b)
--    where i = getInt (value vs a)
--eval vs (PReturn a) = outPut vs a
--eval (PAddToIntArray a b) vs = updateVariables vs (getString a) (PIntArray ((getArray (getVariable vs a)) ++ [getInt (value vs b)]))


--Prints output to stdout
printOutput :: Expr -> EvalState ()
printOutput (PInt a) = liftIO $ putStrLn $ (show $ a)
printOutput (PString a) = liftIO $ putStrLn $ a
printOutput (PTrue) = liftIO $ putStrLn $ "True"
printOutput (PFalse) = liftIO $ putStrLn $ "False"
printOutput (PIntArray a) = liftIO $ putStrLn $ (show $ a)
printOutput a = liftIO $ putStrLn $ (show $ a)

--evaluates the entire program as a list of actions, returning the final environment (the final state)
evalProgs :: Environment -> [Expr] -> EvalState Environment
evalProgs env [] = return env
evalProgs env (expr:rest) = do  newEnv <- eval env expr
                                evalProgs newEnv rest

--evaluates a single action of the program
eval :: Environment -> Expr -> EvalState Environment
eval env (PAssignVar a b c) = do
        let newEnv = updateVariables env (getString b) (value env c)
        return newEnv
eval env (PUpdateVar b c) = do
        let newEnv = updateVariables env (getString b) (value env c)
        return newEnv
eval env (PIf a b) = do
        if (value env a) == PTrue
            then evalProgs env b
            else return env
eval env (PIfElse a b c) = do
        if (value env a) == PTrue
            then evalProgs env b
            else evalProgs env c
eval env (PWhile a b) = do
    if (value env a) == PTrue
        then do progRes <- evalProgs env b
                eval progRes (PWhile a b)
        else return env
eval env (PFor a b) = do
    let i = getInt (value env a)
    if i == 0
        then return env
        else do progRes <- evalProgs env b
                eval progRes (PFor (PInt (i - 1)) b)
eval env (PReturn a) = do
    let expr = value env a
    printOutput expr
    return env
eval env (PAddToIntArray a b) = do
    let newEnv = updateVariables env (getString a) (PIntArray ((getArray (getVariable env a)) ++ [getInt (value env b)]))
    return newEnv


--evaluates the result of an expression using recursive, big-step semantics
value :: Environment -> Expr -> Expr
value vs (PInt a) = (PInt a)
value vs (PString a) = (PString a)
value vs (PIntArray a) = (PIntArray a)
value vs (PVar a) = (getVariable vs (PVar a))
value vs (PNegative a) = PInt (-(getInt (value vs a)))
value vs (PMult a b) = PInt ((getInt (value vs a)) * (getInt (value vs b)))
value vs (PDiv a b) = PInt ((getInt (value vs a)) `div` (getInt (value vs b)))
value vs (PAdd a b) = PInt ((getInt (value vs a)) + (getInt (value vs b)))
value vs (PSub a b) = PInt ((getInt (value vs a)) - (getInt (value vs b)))
value vs (PCompareLessMore (PLength (PVar a)) b) = boolToExpr (infCompOpp (getArray (getVariable vs (PVar a))) (getInt (value vs b)))
value vs (PCompareLessMore a (PLength (PVar b))) = boolToExpr (infComp (getArray (getVariable vs (PVar b))) (getInt (value vs a)))
value vs (PCompareLessMore a b) | (getInt (value vs a)) < (getInt (value vs b)) = PTrue
                                | otherwise = PFalse 
value vs (PCompareMoreLess (PLength (PVar a)) b) = boolToExpr (infComp (getArray (getVariable vs (PVar a))) (getInt (value vs b)))
value vs (PCompareMoreLess a (PLength (PVar b))) = boolToExpr (infCompOpp (getArray (getVariable vs (PVar b))) (getInt (value vs a)))
value vs (PCompareMoreLess a b) | (getInt (value vs a)) > (getInt (value vs b)) = PTrue
                                | otherwise = PFalse
value vs (PCompareEqLessMore (PLength (PVar a)) b) = boolToExpr (not (infComp (getArray (getVariable vs (PVar a))) (getInt (value vs b))))
value vs (PCompareEqLessMore a (PLength (PVar b))) = boolToExpr (not (infCompOpp (getArray (getVariable vs (PVar b))) (getInt (value vs a))))
value vs (PCompareEqLessMore a b) | (getInt (value vs a)) <= (getInt (value vs b)) = PTrue
                                  | otherwise = PFalse
value vs (PCompareEqMoreLess (PLength (PVar a)) b) = boolToExpr (not (infCompOpp (getArray (getVariable vs (PVar a))) (getInt (value vs b))))
value vs (PCompareEqMoreLess a (PLength (PVar b))) = boolToExpr (not (infComp (getArray (getVariable vs (PVar b))) (getInt (value vs a))))
value vs (PCompareEqMoreLess a b) | (getInt (value vs a)) >= (getInt (value vs b)) = PTrue
                                  | otherwise = PFalse
value vs (PNotEqual a b) | (value vs a) /= (value vs b) = PTrue
                         | otherwise = PFalse
value vs (PEqual a b) | (value vs a) == (value vs b) = PTrue
                      | otherwise = PFalse
value vs (PNot a) | (value vs a) == PTrue = PFalse
                  | otherwise = PTrue
value vs (PXor a b) | ((value vs a) == PTrue) && ((value vs b) == PFalse) = PTrue
                    | ((value vs a) == PFalse) && ((value vs b) == PTrue) = PTrue
                    | otherwise = PFalse
value vs (PAnd a b) | ((value vs a) == PTrue) && ((value vs b) == PTrue) = PTrue
                    | otherwise = PFalse
value vs (POr a b) | ((value vs a) == PFalse) && ((value vs b) == PFalse) = PFalse
                   | otherwise = PTrue
value vs (PGetIntValueAt a b) = (PInt ((getArray (getVariable vs a))!!(getInt (value vs b))))
value vs (PGetInputValueAt b) = (PIntArray ((getInArray (getVariable vs (PVar "IN")))!!(getInt (value vs b))))
value vs (PLength (PVar a)) = PInt (length (getArray (getVariable vs (PVar a))))

--updates the environment with the new value by removing any pair that has the same name of the variable
--and then adding the new pair to the list. Adding the variable or if the variable already exists, replacing
--the variable
--We don't need to worry about redeclaration as that is checked in the typechecker
updateVariables :: Environment -> String -> Expr -> Environment
updateVariables [] a b = [(a,b)]
updateVariables xs a b = (filter (\(x,_) -> x /= a) xs) ++ [(a,b)]

getVariable :: Environment -> Expr -> Expr
getVariable [] _ = error "Variable called doesn't exist"
getVariable ((a,b):as) (PVar x) | x == a = b
                                | otherwise = getVariable as (PVar x)


--compares the length of an unbounded list with a number
infComp :: [Int] -> Int -> Bool
infComp [] 0 = False
infComp [] a = False
infComp (x:xs) 0 = True
infComp (x:xs) a = infComp xs (a-1)

--compares the oppsite way
infCompOpp :: [Int] -> Int -> Bool
infCompOpp [] 0 = False
infCompOpp [] a = True
infCompOpp (x:xs) 0 = False
infCompOpp (x:xs) a = infComp xs (a-1)

--converts a bool into its equivalent as an expression of the language
boolToExpr :: Bool -> Expr
boolToExpr True = PTrue
boolToExpr False = PFalse


--These are the functions for getting the true value from an expression after it is evaluated

getString :: Expr -> String
getString (PString a) = a
getString (PVar a) = a

getInt :: Expr -> Int
getInt (PInt a) = a

getArray :: Expr -> [Int]
getArray (PIntArray a) = a

getInArray :: Expr -> [[Int]]
getInArray (PIn a) = a



