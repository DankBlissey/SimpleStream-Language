import ProjectLexer
import ProjectGrammar
import ProjectTypes
import ProjectEval
import System.Environment
import Control.Exception
import System.IO
import Data.List
import Control.Monad.State
import Control.DeepSeq

main :: IO ()
main = catch main' noParse

main' = do  (fileName : _ ) <- getArgs 
            sourceText <- readFile fileName
            --putStrLn ("Parsing : " ++ sourceText)
            let parsedProg = reverse (projectParser (alexScanTokens sourceText))
            --putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
            let ifParsed = show parsedProg
            --putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
            let evalTypes = printTypes (typeChecks [] parsedProg)
            putStr ((evalTypes) `deepseq` (""))
            --putStrLn ("Reading input data and evaluating")
            --input <- readFile inputName
            input <- getContents
            let inputArray = buildArray input
            --putStrLn (show inputArray)
            let startEnvironment = [("IN",(PIn inputArray))]
            final <- execStateT (evalProgs startEnvironment parsedProg) startEnvironment
            putStr ("")
            --let result = answer startEnvironment parsedProg
            --putStrLn ("Evaluates to " ++ (printReturn result) ++ "\n")
            ----putStrLn (printReturn result) 

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

noType :: SomeException -> IO ()
noType e = do   let err =  show e
                hPutStr stderr err
                return ()

--builds an array of the input given, this is done lazily still
buildArray :: String -> [[Int]]
buildArray input = 
    let rows = lines input
        columns = map (map read . words) rows
    in transpose columns
