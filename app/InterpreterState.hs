{-# LANGUAGE RecordWildCards #-}
module InterpreterState where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe, isJust)
import Data.List.Split (splitOn)
import Control.Monad
import Scanner (AlexPosn (AlexPn))

-- TODO check if calling putState is necessary considering that the HashTables are mutable

-- Aux function to emit error messages and finish execution early with err
showPos :: AlexPosn -> String
showPos (AlexPn _ line col) =
    "(Line " ++ show line ++ ", Column " ++ show col ++ ")"

semanticError :: String -> StateType a
semanticError msg = do
    -- liftIO $ putStrLn msg
    fileName <- topImportStack
    parserFail ("[" ++ fileName ++  "] Semantic Error: " ++ msg)
runtimeError :: String -> StateType a
runtimeError msg = do
    fileName <- topImportStack
    parserFail ("[" ++ fileName ++ "] Error: " ++ msg)

initInterpreterState :: String -> IO InterpreterState
initInterpreterState fileName = do
    globalSymbolTable <- liftIO H.new
    importTable <- liftIO H.new

    return InterpreterState
        { globalSymbolTable = globalSymbolTable
        , symbolTableStack = []
        , programState = Starting
        , parserBlock = GlobalScope
        , imports = importTable
        , importStack = [fileName]
        , nestedImportCounter = 0
        , nextScopeId = 1 -- 0 is for the global scope
        , namespaceStack = []
        }


isRunning :: StateType Bool
isRunning = do
    nestedImportCounter <- getNestedImportCounter
    programState <- getProgramState
    return (programState == Running && nestedImportCounter == 0)


pushNamespacePrefix :: String -> StateType ()
pushNamespacePrefix prefix = do
    st@InterpreterState{..} <- getState
    finalNamespace <- getFinalSymbol prefix
    putState st{namespaceStack = finalNamespace : namespaceStack}

pushMultipleNamespacePrefix :: String -> StateType ()
pushMultipleNamespacePrefix path = do
    let namespaceList = splitOn "::" path
    mapM_ pushNamespacePrefix namespaceList

popNamespacePrefix :: StateType ()
popNamespacePrefix = do
    st@InterpreterState{..} <- getState
    case namespaceStack of
        [] -> return ()
        (_ : rest) -> putState st{namespaceStack = rest}


topNamespacePrefix :: StateType String
topNamespacePrefix = do
    InterpreterState{..} <- getState
    case namespaceStack of
        [] -> return ""
        (top : _) -> return top


getNamespaceStack :: StateType [String]
getNamespaceStack = do
    InterpreterState{..} <- getState
    return namespaceStack

setNamespaceStack :: [String] -> StateType ()
setNamespaceStack n = do
  st <- getState
  putState st { namespaceStack = n }

getFinalSymbol :: String -> StateType String
getFinalSymbol symbolId = do
    top <- topNamespacePrefix
    if top == "" 
        then do
            return symbolId
        else return $ top ++ "::" ++ symbolId


addImport :: String -> StateType ()
addImport fileName = do
    st@InterpreterState{..} <- getState
    liftIO $ H.insert imports fileName False
    putState st{ nestedImportCounter = nestedImportCounter + 1
               , importStack = fileName : importStack
               }

finishImport :: String -> StateType()
finishImport fileName = do
    st@InterpreterState{..} <- getState
    liftIO $ H.insert imports fileName True
    putState st{ nestedImportCounter = nestedImportCounter - 1
               , importStack = tail importStack
               }

topImportStack :: StateType String
topImportStack = do
    InterpreterState{..} <- getState
    return $ head importStack

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

changeTopScopeVisibility :: Bool -> StateType ()
changeTopScopeVisibility newAccessModifier = do
    st@InterpreterState{..} <- getState
    case symbolTableStack of
        [] -> return ()
        (top : rest) -> do
            let (table, _, tableId) = top
            putState st
              { symbolTableStack = (table, newAccessModifier, tableId) : rest
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

mergeTableToScope :: SymbolTableType -> StateType ()
mergeTableToScope table = do
    InterpreterState{} <- getState
    destiny <- topScope
    pairs <- liftIO $ H.toList table
    forM_ pairs $ \(k, (s, v)) -> do
        existing <- liftIO $ H.lookup destiny k
        case existing of
            Nothing -> liftIO $ H.insert destiny k (s, v)
            Just (existingList, _) -> do
                liftIO $ H.insert destiny k (existingList ++ s, Nothing)

mergeTableToGlobal :: SymbolTableType -> StateType ()
mergeTableToGlobal table = do
    InterpreterState{..} <- getState
    pairs <- liftIO $ H.toList table
    forM_ pairs $ \(k, (s, v)) -> do
        existing <- liftIO $ H.lookup globalSymbolTable k
        case existing of
            Nothing -> liftIO $ H.insert globalSymbolTable k (s, v)
            Just (existingList, _) -> 
                liftIO $ H.insert globalSymbolTable k (existingList ++ s, Nothing)

insertSymbol :: SymbolType -> AlexPosn -> StateType ()
insertSymbol (symbolId, symbolType, maybeValue) posn = do
    InterpreterState{..} <- getState
    -- Insert on symbol table
    case symbolTableStack of
        [] -> insertInTable globalSymbolTable
        (top : _) -> do
            let (table, _, _) = top
            insertInTable table
    where
        insertInTable
            :: SymbolTableType
            -> StateType ()
        insertInTable table = do
            finalSymbolId <- getFinalSymbol symbolId
            existing <- liftIO $ H.lookup table finalSymbolId

            case symbolType of
                FuncType {} -> do
                    let (existingTypes, existingVal) = fromMaybe ([], Nothing) existing
                    assertEmptyValue existingVal
                    assertEmptyValue maybeValue
                    liftIO $ H.insert table finalSymbolId (existingTypes ++ [symbolType], Nothing)
                ProcType {} -> do
                    let (existingTypes, existingVal) = fromMaybe ([], Nothing) existing
                    assertEmptyValue existingVal
                    assertEmptyValue maybeValue
                    liftIO $ H.insert table finalSymbolId (existingTypes ++ [symbolType], Nothing)
                _ -> do
                    when (isJust existing ) (semanticError $ "Redefinition of \"" ++ symbolId ++ "\"" ++ showPos posn)
                    liftIO $ H.insert table finalSymbolId ([symbolType], maybeValue)



-- TODO remove this before "realising"
-- either [type] has only one element is Nothign
assertCorrectness :: [Type] -> Maybe Value -> StateType ()
assertCorrectness types maybeValue =
    case (types, maybeValue) of
        (_:_:_, Just _) -> liftIO $ putStrLn "<updateSymbolTable>"
        _               -> return ()

walkStack
    :: (SymbolTableType -> String -> IO (Maybe a))
    -> String
    -> SymbolTableStackType
    -> IO (Maybe a)
walkStack _ _ [] = return Nothing
walkStack action symbolId ((table, canAccessParent, _) : rest) = do
    r <- action table symbolId
    case r of
        Just v  -> return (Just v)
        Nothing -> if canAccessParent
                      then walkStack action symbolId rest
                      else return Nothing

walkStackById
    :: Int
    -> (SymbolTableType -> String -> IO (Maybe a))
    -> String
    -> SymbolTableStackType
    -> IO (Maybe a)
walkStackById _ _ _ [] = return Nothing
walkStackById targetId action symbolId ((table, canAccessParent, currId) : rest)
    | targetId == currId = liftIO (action table symbolId)
    | canAccessParent    = walkStackById targetId action symbolId rest
    | otherwise          = return Nothing

walkScopes
    :: String
    -> (SymbolTableType -> String -> IO (Maybe ([Type], Maybe Value)))
    -> StateType (Maybe ([Type], Maybe Value))
walkScopes symbolId action = do
    InterpreterState{..} <- getState
    a <- liftIO $ walkNamespaceStack namespaceStack symbolId symbolTableStack (walkStack action)
    case a of
        Nothing -> liftIO $ walkNamespaceStack namespaceStack symbolId [(globalSymbolTable, False, 0)] (walkStack action) 
        Just e -> return $ Just e

walkScopesById
    :: Int
    -> String
    -> (SymbolTableType -> String -> IO (Maybe ([Type], Maybe Value)))
    -> StateType (Maybe ([Type], Maybe Value))
walkScopesById refId symbolId action = do
    InterpreterState{..} <- getState
    if refId == 0 
        then liftIO $ walkNamespaceStack namespaceStack symbolId [(globalSymbolTable, False, 0)] (walkStackById refId action)
        else liftIO $ walkNamespaceStack namespaceStack symbolId symbolTableStack (walkStackById refId action)

walkNamespaceStack
    :: [String]
    -> String
    -> SymbolTableStackType
    -> (String -> SymbolTableStackType -> IO (Maybe ([Type], Maybe Value)))
    -> IO (Maybe ([Type], Maybe Value))
walkNamespaceStack [] symbolId table action = action symbolId table 
walkNamespaceStack (currNamespace : namespaceTail) symbolId table action = do
    let finalSymbolId = currNamespace ++ "::" ++ symbolId
    stackResult <- action finalSymbolId table 

    case stackResult of
        Just r -> return $ Just r
        Nothing -> walkNamespaceStack namespaceTail symbolId table action

findSymbolTableId :: String -> AlexPosn -> StateType Int
findSymbolTableId symbolId posn = do
    namespaceStack <- getNamespaceStack
    return search symbolId
    where 
        search :: StateType Int
        search = do
            return 1

getSymbolRef :: String -> AlexPosn -> StateType (Type, Maybe Value)
getSymbolRef symbolId posn = do
    (symbolTypeList, _symbolV) <- consultSymbol symbolId posn
    symbolT <- case symbolTypeList of
                    [] -> semanticError $ "trying to get a reference from a non-typed symbol at " ++ showPos posn -- will never reach this
                    (_:_:_) -> semanticError $ "trying to get a reference for a subprogram at " ++ showPos posn
                    (h:_) -> return h
    tableId <- findSymbolTableId symbolId posn
    return (RefType symbolT, Just $ RefValue symbolId tableId) -- TODO check if this is correct


consultSymbolMaybe :: String -> StateType (Maybe ([Type], Maybe Value))
consultSymbolMaybe symbolId = walkScopes symbolId H.lookup

consultSymbol :: String -> AlexPosn -> StateType ([Type], Maybe Value)
consultSymbol symbolId posn = do
    r <- walkScopes symbolId H.lookup
    case r of
        Nothing -> semanticError $ "using non-existing symbol \"" ++ symbolId ++ "\" at " ++ showPos posn
        Just v -> return v

consultSymbolByIdMaybe :: (String, Int) -> StateType (Maybe([Type], Maybe Value))
consultSymbolByIdMaybe (symbolId, tableId) = walkScopesById tableId symbolId H.lookup

consultSymbolById :: (String, Int) -> AlexPosn -> StateType ([Type], Maybe Value)
consultSymbolById (symbolId, tableId) posn = do
    r <- walkScopesById tableId symbolId H.lookup
    case r of
        Nothing -> semanticError $ "\"" ++ symbolId ++ "\"" ++ "doesn't exist in this scope " ++ showPos posn
        Just v -> return v

updateSymbol :: String -> ([Type], Maybe Value) -> StateType ()
updateSymbol symbolId (typeList, value) = do
    let helper table symbolId' = do
            found <- H.lookup table symbolId'
            case found of
                Just _ -> do
                    H.insert table symbolId' (typeList, value)
                    return $ Just (typeList, value)
                Nothing ->
                    return Nothing
        
    result <- walkScopes symbolId helper
    case result of
        Nothing -> semanticError "trying to use undeclared var"
        Just _ -> return ()

updateSymbolById :: (String, Int) -> ([Type], Maybe Value) -> StateType ()
updateSymbolById (symbolId, tableId) (typeList, value) = do
    assertCorrectness typeList value

    let helper table symbolId' = do
            found <- H.lookup table symbolId'
            case found of
                Just _ -> do
                    H.insert table symbolId' (typeList, value)
                    return $ Just (typeList, value)
                Nothing ->
                    return Nothing

    result <- walkScopesById tableId symbolId helper
    case result of
        Nothing -> semanticError "trying to updated invalid reference"
        Just _ -> return ()

