{-# LANGUAGE RecordWildCards #-}
module InterpreterState where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)
import Data.Foldable
import Control.Monad

-- Aux function to emit error messages and finish execution early with err
semanticError :: String -> StateType a
semanticError msg = parserFail ("Semantic Error: " ++ msg)
runtimeError :: String -> StateType a
runtimeError msg = parserFail ("Error: " ++ msg)

initInterpreterState :: IO InterpreterState
initInterpreterState = do
    globalMemory <- liftIO H.new
    globalSymbolTable <- liftIO H.new

    return InterpreterState 
        { globalMemoryTable=globalMemory
        , globalSymbolTable=globalSymbolTable
        , memoryTableStack=[]
        , symbolTableStack=[]
        , programState=Starting
        , parserBlock=GlobalScope
        }
    

isRunning :: StateType Bool
isRunning = do
    programState <- getProgramState
    return (programState == Running)

openScope :: Bool -> StateType () 
openScope canAccessParentTables = do
    st@InterpreterState{..} <- getState
    newTable <- liftIO H.new -- st stands for symbol table
    newMemory <- liftIO H.new 
    putState st {
        symbolTableStack = (newTable, canAccessParentTables) : symbolTableStack,
        memoryTableStack = newMemory : memoryTableStack
        }

closeScope :: StateType ()
closeScope = do
    st@InterpreterState{..} <- getState

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

topScope :: StateType SymbolTableType
topScope = do
    InterpreterState{..} <- getState
    case symbolTableStack of
        [] -> return globalSymbolTable
        (top : _) -> do
            let (table, _) = top
            return table

getProgramState :: StateType ProgramState
getProgramState = programState <$> getState

setProgramState :: ProgramState -> StateType ()
setProgramState pst = do
  st <- getState
  putState st { programState = pst }

getParserBlock :: StateType ParserBlock
getParserBlock = parserBlock <$> getState

setParserBlock :: ParserBlock -> StateType ()
setParserBlock pst = do
  st <- getState
  putState st { parserBlock = pst }

addImplMethods :: SymbolType -> StateType ()
addImplMethods (symbolId, NamespaceType st) = do
    result <- consultSymbol symbolId
    case result of
        Nothing -> semanticError "TODO How did we get here ??????" 
        Just typeList -> findUpdateNamespace typeList []
    where 
        findUpdateNamespace (ImplNamespaceType currSt:t) carry = do
            liftIO $ mergeSymbolTables currSt st
            updateSymbol symbolId $ carry ++ [NamespaceType currSt] ++ t
        findUpdateNamespace (h:t) carry = findUpdateNamespace t (h:carry)
        findUpdateNamespace [] _ = 
            insertSymbol (symbolId, ImplNamespaceType st) True
addImplMethods (_, _) = fail "TODO write errmsg: should not get into this"

mergeSymbolTables :: SymbolTableType -> SymbolTableType -> IO ()
mergeSymbolTables dst src = do
    pairs <- liftIO $ H.toList src
    forM_ pairs $ \(k, v) -> do
        existing <- liftIO $ H.lookup dst k
        case existing of
            Nothing -> liftIO $ H.insert dst k v
            Just existingList -> liftIO $ H.insert dst k (existingList ++ v)
            
updateSymbol :: String -> [Type] -> StateType ()
updateSymbol symbolId typeList = do
    st@InterpreterState{..} <- getState
    case symbolTableStack of
        [] -> do
            liftIO $ H.insert globalSymbolTable symbolId typeList
            putState st { globalSymbolTable = globalSymbolTable }
        (top : rest) -> do
            let (table, b) = top
            liftIO $ H.insert table symbolId typeList
            putState st { symbolTableStack = (table, b):rest}

insertSymbol :: SymbolType -> Bool -> StateType ()
insertSymbol (symbolId, symbolType) canBeDuplicate = do
    st@InterpreterState{..} <- getState
    -- Insert on symbol table
    case symbolTableStack of
        [] -> do
            existingSymbol <- liftIO $ H.lookup globalSymbolTable symbolId
            if canBeDuplicate then
                liftIO $ H.insert globalSymbolTable symbolId $ fromMaybe [] existingSymbol ++ [symbolType]
                else liftIO $ H.insert globalSymbolTable symbolId [symbolType]
            putState st { globalSymbolTable = globalSymbolTable }
        (top : rest) -> do
            let (table, b) = top
            existingSymbol <- liftIO $ H.lookup table symbolId
            if canBeDuplicate then
                liftIO $ H.insert table symbolId $ fromMaybe [] existingSymbol ++ [symbolType]
                else liftIO $ H.insert table symbolId [symbolType]
            putState st { symbolTableStack = (table, b):rest}
    -- Insert on memory table
    case memoryTableStack of
        [] -> do
            liftIO $ H.insert globalMemoryTable symbolId Nothing
            putState st { globalMemoryTable = globalMemoryTable }
        (top : rest) -> do
            liftIO $ H.insert top symbolId Nothing
            putState st { memoryTableStack = top:rest}

updateValue :: MemoryType -> StateType ()
updateValue (symbolId, value) = do
    InterpreterState{..} <- getState
    result <- update (symbolId, value) memoryTableStack
    unless result $ do
        maybeValue <- liftIO $ H.lookup globalMemoryTable symbolId
        case maybeValue of
            Nothing -> fail $ "updateValue " ++ symbolId
            Just _ -> liftIO $ H.insert globalMemoryTable symbolId (Just value)
  where
    update :: MemoryType -> MemoryTableStackType -> StateType Bool
    update (_, _) [] = return False
    update (n, v) (top:rest) = do
        found <- liftIO $ H.lookup top n
        case found of
            Just _ -> do
                liftIO $ H.insert top n (Just v)
                return True
            Nothing -> update (n, v) rest

consultSymbol :: String -> StateType (Maybe [Type])
consultSymbol symbol = do
    InterpreterState{..} <- getState
    result <- liftIO $ search symbol symbolTableStack
    -- If not found on table stack, search on global table
    case result of
        Nothing -> liftIO $ H.lookup globalSymbolTable symbol
        Just _ -> return result
  where
    search :: String -> SymbolTableStackType -> IO (Maybe [Type])
    search _ [] = return Nothing
    search name ((table, canAccessParent):rest) = do
        result <- H.lookup table name
        case result of
            Just ty -> return (Just ty)
            Nothing ->
                if canAccessParent then
                    search name rest
                else return Nothing

consultValue :: String -> StateType (Maybe Value)
consultValue symbol = do
    InterpreterState{..} <- getState
    result <- liftIO $ search symbol memoryTableStack
    -- If not found on memory stack, search on global memory
    case result of
        Nothing -> do
            v <- liftIO $ H.lookup globalMemoryTable symbol
            case v of
                Nothing -> return Nothing
                Just v' -> return v'
        Just _ -> return result
  where
    search :: String -> MemoryTableStackType -> IO (Maybe Value)
    search _ [] = return Nothing
    search name (table:rest) = do
        result <- H.lookup table name
        case result of
            Just ty -> return ty
            Nothing -> search name rest
