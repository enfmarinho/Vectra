module Main where

import System.Environment (getArgs)
import Parser
import Scanner
import System.Exit (exitSuccess)

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [filename] -> do
            lexicalResult <- getTokens filename
            tokenList <- case lexicalResult of
                Left _ -> do
                    exitSuccess -- End execution early since a lexical error has been found
                Right tokenList -> return tokenList
            
            sintaxResult <- parser tokenList
            case sintaxResult of
                Left err -> print err
                Right _ -> return ()

        _ -> putStrLn "Missing file to interprete, pass its path as a command line argument!"
