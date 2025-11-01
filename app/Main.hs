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
            lexical_result <- getTokens filename
            token_list <- case lexical_result of
                Left _ -> do
                    exitSuccess -- End execution early since a lexical error has been found
                Right token_list -> return token_list
            
            sintax_result <- parser token_list
            case sintax_result of
                Left err -> print err
                Right _ -> return ()

        _ -> putStrLn "Missing file to interprete, pass its path as a command line argument!"
