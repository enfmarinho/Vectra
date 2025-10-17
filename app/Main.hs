module Main where

import qualified Data.HashTable.IO as H
import SymbolTable
import System.Environment (getArgs)
import Parser
import Scanner
import qualified Data.Foldable as H

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [filename] -> do
            global_symbol_table <- H.new :: IO SymbolTableType
            let stack = [global_symbol_table]

            token_list <- getTokens filename
            result  <- parser token_list stack
            case result of
                Left err -> print err
                Right ans -> print ans

        _ -> putStrLn "Missing file to interprete, pass its path as a command line argument!"
