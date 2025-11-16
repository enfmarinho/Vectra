{-# LANGUAGE RecordWildCards #-}
module InterpreterState where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe, isJust)
import Control.Monad

-- TODO check if calling putState is necessary considering that the HashTables are mutable

-- Aux function to emit error messages and finish execution early with err
semanticError :: String -> StateType a
semanticError msg = parserFail ("Semantic Error: " ++ msg)
runtimeError :: String -> StateType a
runtimeError msg = parserFail ("Error: " ++ msg)

initInterpreterState :: IO InterpreterState
initInterpreterState = do
    globalSymbolTable <- liftIO H.new
    importTable <- liftIO H.new

    return InterpreterState
        { globalSymbolTable = globalSymbolTable
        , symbolTableStack = []
        , programState = Starting
        , parserBlock = GlobalScope
        , imports = importTable
        , nestedImportCounter = 0
        , nextScopeId = 0
        }


isRunning :: StateType Bool
isRunning = do
    nestedImportCounter <- getNestedImportCounter
    programState <- getProgramState
    return (programState == Running && nestedImportCounter == 0)

addImport :: String -> StateType ()
addImport fileName = do
    st@InterpreterState{..} <- getState
    liftIO $ H.insert imports fileName False
    putState st{nestedImportCounter = nestedImportCounter + 1}

finishImport :: String -> StateType()
finishImport fileName = do
    st@InterpreterState{..} <- getState
    liftIO $ H.insert imports fileName True
    putState st{nestedImportCounter = nestedImportCounter - 1}

searchImport :: String -> StateType (Maybe Bool)
searchImport fileName = do
    InterpreterState{..} <- getState
    liftIO $ H.lookup imports fileName

pushScope :: SymbolTableType -> Bool -> StateType ()
pushScope table canAccessParentTables = do
    st@InterpreterState{..} <- getState
    putState st {
            symbolTableStack = (table, canAccessParentTables, nextScopeId) : symbolTableStack,
            nextScopeId = nextScopeId + 1
        }

openScope :: Bool -> StateType ()
openScope canAccessParentTables = do
    st@InterpreterState{..} <- getState
    newTable <- liftIO H.new
    putState st {
            symbolTableStack = (newTable, canAccessParentTables, nextScopeId) : symbolTableStack,
            nextScopeId = nextScopeId + 1
        }

closeScope :: StateType ()
closeScope = do
    st@InterpreterState{..} <- getState

    let newSymbolStack = case symbolTableStack of
                                []        -> []  -- nothing to pop
                                (_:rest)  -> rest

    putState st
      { symbolTableStack = newSymbolStack
      }

topScope :: StateType SymbolTableType
topScope = do
    InterpreterState{..} <- getState
    case symbolTableStack of
        [] -> return globalSymbolTable
        (top : _) -> do
            let (table, _, _) = top -- TODO
            return table

getExpectedReturnT :: StateType (Maybe Type)
getExpectedReturnT = do
    st <- getParserBlock
    case st of
        Method t -> return t
        Conditional t -> return t
        Loop t -> return t
        _ -> return Nothing

getNestedImportCounter :: StateType Int
getNestedImportCounter = nestedImportCounter <$> getState

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

assertEmptyValue :: Maybe Value -> StateType ()
assertEmptyValue m = do
    case m of
        Nothing -> return ()
        Just _ -> semanticError "TODO 3"

insertSymbol :: SymbolType -> Bool -> StateType ()
insertSymbol (symbolId, symbolType, maybeValue) canBeDuplicate = do
    st@InterpreterState{..} <- getState
    -- Insert on symbol table
    case symbolTableStack of
        [] -> do
            existingSymbol <- liftIO $ H.lookup globalSymbolTable symbolId
            if canBeDuplicate then do
                let (existingSymbolTypeList, existingSymbolMaybeValue) = fromMaybe ([], Nothing) existingSymbol
                assertEmptyValue existingSymbolMaybeValue
                assertEmptyValue maybeValue

                liftIO $ H.insert globalSymbolTable symbolId  (existingSymbolTypeList ++ [symbolType], Nothing)
            else
                liftIO $ H.insert globalSymbolTable symbolId ([symbolType], maybeValue)

            putState st { globalSymbolTable = globalSymbolTable }
        (top : rest) -> do
            let (table, b, scopeId) = top
            existingSymbol <- liftIO $ H.lookup table symbolId
            if canBeDuplicate then do
                let (existingSymbolTypeList, existingSymbolMaybeValue) = fromMaybe ([], Nothing) existingSymbol
                assertEmptyValue existingSymbolMaybeValue
                assertEmptyValue maybeValue

                liftIO $ H.insert table symbolId (existingSymbolTypeList ++ [symbolType], Nothing)
            else
                liftIO $ H.insert table symbolId ([symbolType], maybeValue)

            putState st { symbolTableStack = (table, b, scopeId):rest}


-- TODO remove this before "realising"
-- either [type] has only one element is Nothign
assertCorrectness :: [Type] -> Maybe Value -> StateType ()
assertCorrectness types maybeValue =
    case (types, maybeValue) of
        (_:_:_, Just _) -> liftIO $ putStrLn "<updateSymbolTable>"
        _               -> return ()


updateSymbolTable :: String -> ([Type], Maybe Value) -> StateType ()
updateSymbolTable symbolId (typeList, value) = do
    st@InterpreterState{..} <- getState
    assertCorrectness typeList value
    result <- searchUpdate symbolTableStack
    unless result $ do
        maybeValue <- liftIO $ H.lookup globalSymbolTable symbolId
        case maybeValue of
            Nothing -> fail $ "<updateSymbolTable> symbolId: " ++ symbolId
            Just _ -> liftIO $ H.insert globalSymbolTable symbolId (typeList, value)

    case symbolTableStack of
        [] -> do
            liftIO $ H.insert globalSymbolTable symbolId (typeList, value)
            putState st { globalSymbolTable = globalSymbolTable }
        (top : rest) -> do
            let (table, b, scopeId) = top
            liftIO $ H.insert table symbolId (typeList, value)
            putState st { symbolTableStack = (table, b, scopeId):rest}
    where
        searchUpdate :: SymbolTableStackType -> StateType Bool
        searchUpdate ((table, canAccessParent, _):rest) = do
            lookupResult <- liftIO $ H.lookup table symbolId
            if isJust lookupResult then do
                liftIO $ H.insert table symbolId (typeList, value)
                return True
            else if canAccessParent then
                searchUpdate rest
            else return False
        searchUpdate _ = return False


searchUpdateSymbolTable :: (String, Int) -> ([Type], Maybe Value) -> StateType ()
searchUpdateSymbolTable (symbolId, tableId) (typeList, value) = do
    InterpreterState{..} <- getState
    assertCorrectness typeList value
    searchUpdate symbolTableStack
    where
        searchUpdate :: SymbolTableStackType -> StateType ()
        searchUpdate ((table, canAccessParent, currTableId):rest) =
            if currTableId == tableId then do
                lookupResult <- liftIO $ H.lookup table symbolId
                case lookupResult of
                    Just ([_currTypeList], _) -> do
                        -- TODO assert currTypeList is equivalent to typeList
                        liftIO $ H.insert table symbolId (typeList, value)
                        -- TODO do i need a putState ? 
                    _ -> semanticError "trying to use a invalid reference"
            else when canAccessParent $ searchUpdate rest
        searchUpdate _ = semanticError "trying to use a invalid reference"


consultSymbolTable :: String -> StateType (Maybe ([Type], Maybe Value))
consultSymbolTable symbol = do
    InterpreterState{..} <- getState
    result <- liftIO $ search symbol symbolTableStack
    -- If not found on table stack, search on global table
    case result of
        Nothing -> liftIO $ H.lookup globalSymbolTable symbol
        Just _ -> return result
  where
    search :: String -> SymbolTableStackType -> IO (Maybe ([Type], Maybe Value))
    search _ [] = return Nothing
    search name ((table, canAccessParent, _scopeId):rest) = do
        result <- H.lookup table name
        case result of
            Just ty -> return (Just ty)
            Nothing ->
                if canAccessParent then
                    search name rest
                else return Nothing

consultSymbolTableById :: (String, Int) -> StateType (Maybe ([Type], Maybe Value))
consultSymbolTableById (symbolId, tableId) = do
    InterpreterState{..} <- getState
    searchUpdate symbolTableStack
    where
        searchUpdate :: SymbolTableStackType -> StateType (Maybe ([Type], Maybe Value))
        searchUpdate ((table, canAccessParent, currTableId) : rest)
          | currTableId == tableId = do
                lookupResult <- liftIO $ H.lookup table symbolId
                case lookupResult of
                    Just r -> return $ Just r
                    _ -> semanticError "trying to use a invalid reference"
          | canAccessParent = searchUpdate rest
          | otherwise = semanticError "tyring to use a invalid reference"
        searchUpdate _ = semanticError "trying to use a invalid reference"
