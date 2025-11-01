{-# LANGUAGE RecordWildCards #-}
module ParserState
  ( SymbolType
  , SymbolTableType
  , SymbolTableStackType
  , StateType
  , initParserState
  , openScope
  , closeScope
  , insertSymbol
  , insertValue
  , updateValue
  , consultSymbol
  ) where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy

initParserState :: IO ParserState
initParserState = do
    globalMemory <- liftIO H.new
    globalSymbolTable <- liftIO H.new

    return ParserState 
        { globalMemoryTable=globalMemory
        , globalSymbolTable=globalSymbolTable
        , memoryTableStack=[]
        , symbolTableStack=[]
        , isRunning=False
        }
    

openScope :: Bool -> StateType () 
openScope canAccessParentTables = do
    st@ParserState{..} <- getState
    newTable <- liftIO H.new -- st stands for symbol table
    newMemory <- liftIO H.new 
    putState st {
        symbolTableStack = (newTable, canAccessParentTables) : symbolTableStack,
        memoryTableStack = newMemory : memoryTableStack
        }

closeScope :: StateType ()
closeScope = do
    st@ParserState{..} <- getState

    let newSymbolStack =
          case symbolTableStack of
            []        -> []  -- nothing to pop
            (_:rest)  -> rest

    let newMemoryStack =
          case memoryTableStack of
            []        -> []  -- nothing to pop
            (_:rest)  -> rest

    putState st
      { symbolTableStack = newSymbolStack
      , memoryTableStack = newMemoryStack
      }

insertSymbol :: SymbolType -> StateType ()
insertSymbol (symbolId, symbolType) = do
    st@ParserState{..} <- getState
    case symbolTableStack of
        [] -> do
            liftIO $ H.insert globalSymbolTable symbolId symbolType
            putState st { globalSymbolTable = globalSymbolTable }
        (top : rest) -> do
            let (table, b) = top
            liftIO $ H.insert table symbolId symbolType
            putState st { symbolTableStack = (table, b):rest}

insertValue :: MemoryType -> StateType ()
insertValue (symbolId, value) = do
    st@ParserState{..} <- getState
    case memoryTableStack of
        [] -> do
            liftIO $ H.insert globalMemoryTable symbolId value
            putState st { globalMemoryTable = globalMemoryTable }
        (top : rest) -> do
            liftIO $ H.insert top symbolId value
            putState st { memoryTableStack = top:rest}

updateValue :: MemoryType -> StateType ()
updateValue (symbolId, value) = do
    ParserState{..} <- getState
    liftIO $ update (symbolId, value) memoryTableStack
  where
    update :: MemoryType -> MemoryTableStackType -> IO ()
    update (_, _) [] = return ()
    update (n, v) (top:rest) = do
        found <- H.lookup top n
        case found of
            Just _ -> H.insert top n v
            Nothing -> update (n, v) rest

consultSymbol :: String -> StateType (Maybe Type)
consultSymbol symbol = do
    ParserState{..} <- getState
    liftIO $ search symbol symbolTableStack
  where
    search :: String -> SymbolTableStackType -> IO (Maybe Type)
    search _ [] = return Nothing
    search name ((table, canAccessParent):rest) = do
        result <- H.lookup table name
        case result of
            Just ty -> return (Just ty)
            Nothing ->
                if canAccessParent then
                    search name rest
                else return Nothing

