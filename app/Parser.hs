{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Parser
  ( parser
  ) where

import InterpreterState
import qualified TerminalTokens as TT
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V
import Scanner
import Text.Parsec
import Types
import Assert
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import VectraLib
import Data.List (genericLength)

-- TODO some functions shouldn't return [Token], but yet do. Fix this...

importFile :: String -> AlexPosn -> StateType ()
importFile filePath _posn = do
    result <- searchImport filePath
    case result of
        Nothing -> return ()
        Just b -> unless b $ semanticError $ "cyclic importing " ++ filePath

    addImport filePath
    currState <- getState

    fileTokens <- liftIO $ getTokens filePath
    importTokens <- case fileTokens of
                        Left _ -> fail $ "syntax failure on file " ++ filePath
                        Right t -> return t

    parserResult <- liftIO $ runParserT vectraLanguage currState "<import>" importTokens
    case parserResult of
        Left _ -> fail $ "semantic failure on file " ++ filePath
        Right (_, finalState) -> do

            putState finalState
            finishImport filePath

vectraLanguage :: StateType ([Token], InterpreterState)
vectraLanguage = do
    a <- concat <$> (importCommand `sepEndBy` TT.newLine)
    b <- concat <$> (globalDecl `sepEndBy` TT.newLine)

    currState <- getState
    return (a ++ b, currState)

    where
        importCommand :: StateType [Token]
        importCommand = do
            a <- TT.kwImport
            b <- importList `sepEndBy1` TT.kwComma
            return $ a:b
            where importList = do
                    a <- TT.id
                    let ID posn symbolId = a
                    importSpecialMethod symbolId posn
                    return a
                    <|> do
                        a <- TT.stringLiteral
                        let STRING_LITERAL posn filePath = a
                        importFile filePath posn
                        return a

        globalDecl :: StateType [Token]
        globalDecl = do
            structDecl
            <|> implDecl
            <|> enumDecl
            <|> methodDecl
            <|> varDecl
            <|> namespaceDecl

templateDecl :: StateType ([Token], [String])
templateDecl = do
    a <- TT.opSmaller
    (b, bId) <- idSymbol
    c <- many $ do
        c <- TT.kwComma
        (d, dParam) <- idSymbol
        return (c:[d], dParam)
    d <- TT.opGreater

    let cTokens = b : concatMap fst c
    let cIds = bId : map snd c

    return ([a] ++ [b] ++ cTokens ++ [d], bId : cIds)
  where
    idSymbol :: StateType (Token, String)
    idSymbol = do
        a <- TT.id
        let ID _posn symbolId = a
        insertSymbol (symbolId, TemplateType, Nothing) False
        return (a, symbolId)

templateInstantiation :: StateType ([Token], [Type])
templateInstantiation = do
    _ <- TT.opSmaller
    (a, aType) <- typeStmt
    rest <- many $ do
        b <- TT.kwComma
        (c, cType) <- typeStmt
        return (b:c, cType)
    let tokenList = a ++ concatMap fst rest
        typeList = aType : map snd rest
    _ <- TT.opGreater
    return (tokenList, typeList)

insertTemplateInstantiation :: [Type] -> [String] -> StateType ()
insertTemplateInstantiation [] [] = return ()
insertTemplateInstantiation (_:_) [] = semanticError "Template instantiation: missing symbols"
insertTemplateInstantiation [] (_:_) = semanticError "Template instantiation: missing types"
insertTemplateInstantiation (t:typeRest) (s:symbolRest) = do
    insertSymbol (s, t, Nothing) False
    insertTemplateInstantiation typeRest symbolRest

structDecl :: StateType [Token]
structDecl = do
    openScope False
    _ <- TT.kwStruct
    (a, templateIds) <- option ([], []) templateDecl
    b <- TT.id

    let ID posn symbolId = b
    assertNonAmbiguous symbolId posn

    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent

    publicTable <- liftIO H.new 
    privateTable <- liftIO H.new 
    _ <- many1 (do
                    accessModifier <- option Public (do
                                            _ <- TT.kwPublic
                                            return Public
                                        <|> do
                                            _ <- TT.kwPrivate
                                            return Private
                                        )
                    openScope False
                    _ <- varDecl
                    currScope <- topScope
                    closeScope

                    case accessModifier of
                        Public -> liftIO $ mergeTablesInPlace currScope publicTable
                        Private -> liftIO $ mergeTablesInPlace currScope privateTable
                        _ -> fail "<structDecl>" -- Cannot reach this, just to avoid warnings
                    )
    _ <- TT.newLine
    _ <- TT.unindent
    closeScope
    insertSymbol (symbolId, StructType templateIds publicTable privateTable, Nothing) False
    return $ a ++ [b]

implDecl :: StateType [Token]
implDecl = do
    openScope False
    _ <- TT.kwImpl
    a <- TT.id
    let ID _posn symbolId = a
    result <- consultSymbolTable symbolId
    (publicMethodTable, privateMethodTable, staticMethodTable) <- case result of
                                    Nothing -> semanticError $ "using a impl for a non declared symbol: " ++ symbolId
                                    Just (t, _) -> do helperF t symbolId

    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    _ <- concat <$> many1 (do
                            f <- option Public (do 
                                                _ <- TT.kwStatic
                                                return Static
                                            <|> do
                                                _ <- TT.kwPrivate
                                                return Private
                                            <|> do
                                                _ <- TT.kwPublic
                                                return Public
                                            )

                            -- TODO this is bugged, because a static method should be able to see other static methods,
                            --      at the moment it doesn't 
                            let canAccessStructData = f /= Static
                            openScope canAccessStructData -- open temporary scope 
                            g <- varDecl 
                                <|> methodDecl
                                <|> enumDecl
                                <|> structDecl
                                <|> implDecl
                                <|> namespaceDecl
                            currScope <- topScope 
                            closeScope -- close temporary scope 

                            case f of
                                Public -> liftIO $ mergeTablesInPlace currScope publicMethodTable
                                Private -> liftIO $ mergeTablesInPlace currScope privateMethodTable
                                Static -> liftIO $ mergeTablesInPlace currScope staticMethodTable
                            return g
                        )
    _ <- TT.newLine
    _ <- TT.unindent

    closeScope -- closing scope for static methods
    closeScope -- closing scope for private methods
    closeScope -- closing scope for public methods
    closeScope -- closing scope for private data 
    closeScope -- closing scope for public data

    when (isNothing result) $
        insertSymbol (symbolId, ImplType publicMethodTable privateMethodTable staticMethodTable, Nothing) True 

    return []
    where 
        helperF :: [Type] -> String -> StateType (SymbolTableType, SymbolTableType, SymbolTableType )
        helperF [StructType templateList publicDataTable privateDataTable] symbolId = do
            emptyTable <- liftIO H.new 
            helperF [StructType templateList publicDataTable privateDataTable, 
                     ImplType emptyTable emptyTable emptyTable] symbolId
        helperF [ImplType publicMethodTable privateMethodTable staticMethodTable, 
                 StructType templateList publicDataTable privateDataTable] symbolId = do
            helperF [StructType templateList publicDataTable privateDataTable,
                     ImplType publicMethodTable privateMethodTable staticMethodTable] symbolId
        helperF [StructType templateList publicDataTable privateDataTable, 
                 ImplType publicMethodTable privateMethodTable staticMethodTable] _symbolId = do
            pushScope publicDataTable True
            insertTemplates templateList
            pushScope privateDataTable True
            pushScope publicMethodTable True
            pushScope privateMethodTable True
            pushScope staticMethodTable True
            return (publicMethodTable, privateMethodTable, staticMethodTable)
        helperF _ symbolId = semanticError $ "using a impl for a non-struct type " ++ symbolId
            
        insertTemplates :: [String] -> StateType ()
        insertTemplates [] = return ()
        insertTemplates (h:t) = do
            insertSymbol (h, TemplateType, Nothing) False
            insertTemplates t

namespaceDecl :: StateType [Token]
namespaceDecl = do
    a <- TT.kwNamespace
    b <- TT.id
    let ID _posn symbolId = b
    result <- consultSymbolTable symbolId
    (publicTable, privateTable) <- case result of
            Nothing -> do
                openScope False
                openScope False
                emptyTable <- liftIO H.new 
                return (emptyTable, emptyTable)
            Just (t, _) -> case t of
                                [NamespaceType publicTable privateTable] -> do
                                    pushScope publicTable True
                                    pushScope privateTable True
                                    return (publicTable, privateTable)
                                _ -> semanticError $ "ambiguos declaration, " ++ symbolId ++ " is already declared as another type"
    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    f <- concat <$> many1 (do
                            f <- option Public (do 
                                                _ <- TT.kwPrivate
                                                return Private
                                            <|> do
                                                _ <- TT.kwPublic
                                                return Public
                                            )

                            openScope True -- open temporary scope 
                            g <- varDecl 
                                <|> methodDecl
                                <|> enumDecl
                                <|> structDecl
                                <|> implDecl
                                <|> namespaceDecl
                            currScope <- topScope 
                            closeScope -- close temporary scope 

                            case f of
                                Public -> liftIO $ mergeTablesInPlace currScope publicTable
                                Private -> liftIO $ mergeTablesInPlace currScope privateTable
                                _ -> fail "<namespaceDecl>" -- will never reach this, just to avoid warnings
                            return g
                        )
    _ <- TT.unindent
    _ <- TT.newLine
    closeScope -- close scope for past private declarations
    closeScope -- close scope for past public declarations
    updateSymbolTable symbolId ([NamespaceType publicTable privateTable], Nothing)
    return ([a] ++  [b] ++ [c] ++ [d] ++ [e] ++ f)

enumDecl :: StateType [Token]
enumDecl = do
    _ <- TT.kwEnum
    a <- TT.id

    let ID posn symbolId = a
    assertNonAmbiguous symbolId posn

    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    b <- idList

    let ID _posn enumId = a
    insertSymbol (enumId, EnumType b, Nothing) False
    return [a]
    where
        idList :: StateType [String]
        idList = do
            concat <$> (ids `sepEndBy1` TT.newLine)
            where ids = do
                    a <- TT.id
                    let ID _posn symbolId = a
                    return [symbolId]

optParamDeclList :: StateType ([Token], [(String, Type)])
optParamDeclList = option ([], []) paramDeclList

paramDeclList :: StateType ([Token], [(String, Type)])
paramDeclList = do
    (a, aParam) <- paramDecl
    rest <- many $ do
        b <- TT.kwComma
        (c, cParam) <- paramDecl
        return (b:c, cParam)
    let tokenList = a ++ concatMap fst rest
        paramList = aParam : map snd rest
    return (tokenList, paramList)
    where
        paramDecl :: StateType ([Token], (String, Type))
        paramDecl = do
            (a, varType) <- typeStmt
            b <- TT.id
            let ID _posn symbolId = b
            insertSymbol (symbolId, varType, Nothing) False -- TODO insert correct value in case running
            return (a ++ [b], (symbolId, varType))

methodDecl :: StateType [Token]
methodDecl = do
    previousParserBlock <- getParserBlock
    openScope False
    a <- TT.kwProc
        <|> TT.kwFunc
    (bTokens, bIds) <- option ([], []) templateDecl
    c <- TT.id

    _ <- TT.openParen
    (dTokens, dParams) <- optParamDeclList
    e <- TT.closeParen
    optionF <- optionMaybe returnDecl

    case optionF of
        Nothing -> setParserBlock (Method Nothing)
        Just (_, t) -> setParserBlock (Method $ Just t)

    let ID posn symbolId = c
    when (symbolId == "main") $ do
        programState <- getProgramState
        when (programState == Finished) $ semanticError $ "A second main method is declared here, it must exist only one " ++ showPos posn

        nestedImportCounter <- getNestedImportCounter
        when (nestedImportCounter > 0) $
            semanticError $ "importing a file that contains a main method, an imported file should not contain it " ++ showPos posn

        returnT <- getExpectedReturnT
        case returnT of
            Nothing -> return ()
            Just t | t `elem` [IntType, BoolType] -> return ()
                   | otherwise -> semanticError "main method must either return an int, a bool, or be a procedure"

        setProgramState Running

    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    (g, _) <- stmtList
    _ <- TT.unindent

    closeScope
    assertMethodDeclNotAmbiguous symbolId (map snd dParams) posn
    case a of
        KW_PROC _ -> do
            when (isJust optionF) $ semanticError $
                "A procedure cannot return a value, only functions can. Considerer declaring " ++ symbolId ++
                " as a functions instead " ++ showPos posn

            insertSymbol (symbolId, ProcType bIds dParams g, Nothing) True
        KW_FUNC _ -> case optionF of
                        Nothing -> semanticError $ "A function must return something. Consider declaring "
                                                    ++ symbolId ++ " as a procedure instead " ++ showPos posn
                        Just (_, returnType) -> insertSymbol (symbolId, FuncType bIds dParams returnType g, Nothing) True
        _ ->  fail "<methodDecl>" -- Impossible to get here, this is just to avoid warnings  

    when (symbolId == "main") $ do
        currProgramState <- getProgramState
        case currProgramState of
            Return maybeTV -> do
                case maybeTV of
                    Nothing -> return ()
                    Just (_returnT, returnV) -> case returnV of 
                                                    IntValue v -> when (v /= 0) $ warningMsg "main returned a non-zero int"
                                                    BoolValue v -> unless v $ warningMsg "main return false"
                                                    _ -> return ()
            _ -> return ()
        setProgramState Finished
    setParserBlock previousParserBlock
    return ([a] ++ bTokens ++ [c] ++ dTokens ++ [e] ++ maybe [] fst optionF ++ g)

arrayDecl :: StateType [Token]
arrayDecl = do
    a <- TT.openBracket
    optionB <- optionMaybe expStmt
    c <- TT.closeBracket

    let OPEN_BRACKET posn = a
    case optionB of
        Nothing -> return $ a:[c]
        Just (b, t, _v) -> do
            assertNumberType t posn
            return ([a] ++ b ++ [c])

    where
        assertNumberType :: Type -> AlexPosn -> StateType ()
        assertNumberType value posn = do
            case value of
                IntType -> return ()
                _ -> semanticError $ "Array size should be either empty or a int type " ++ showPos posn

returnDecl :: StateType ([Token], Type)
returnDecl = do
    _ <- TT.opSub
    _ <- TT.opGreater
    (a, aType) <- typeStmt
    optionB <- optionMaybe arrayDecl
    (b, returnType) <- case optionB of
        Nothing -> return ([], aType)
        Just b -> return (b, ArrayType aType)
    return (a ++ b, returnType)

-- TODO make arrayDecl recursive to allow multiple dimension arrays
varDecl :: StateType [Token]
varDecl = do
    a <- optionMaybe TT.kwLocal
    (b, bType) <- typeStmt
    c <- TT.id
    optionD <- optionMaybe arrayDecl
    (d, varType) <- case optionD of
                        Nothing -> return ([], bType)
                        Just d -> return (d, ArrayType bType)
    let ID posn symbolId = c
    when (isNothing a) $ checkShadowing symbolId posn
    insertSymbol (symbolId, varType, Nothing) False
    e <- do
            e <- TT.kwAssingment
            (f, expType, maybeExpValue) <- expStmt
            isRunning' <- isRunning
            when isRunning' $ do
                case maybeExpValue of
                    Nothing -> runtimeError $ "trying to use unitialized variable " ++ showPos posn
                    Just v -> do
                        finalValue <- castValueToType varType (expType, v) posn
                        updateSymbolTable symbolId ([varType], Just finalValue)
            return $ e:f
        <|> return []

    return (b ++ [c] ++ d ++ e)

-- memberAccess :: StateType ([Token], [String])
-- memberAccess = do
--     a <- TT.kwDot
--     b <- TT.id
--     (ct, cs) <- option ([], []) memberAccess
--
--     let ID _posn symbolId = b
--     return ([a] ++ [b] ++ ct, symbolId:cs)

-- TODO var is incomplete, this is just a STUB
var :: StateType ([Token], String, [Type], Maybe Value)
var = do
    a <- TT.id
    -- (b, symbolList) <- option ([], []) memberAccess

    let ID posn symbolId = a
    t <- consultTypeList symbolId posn
    isRunning' <- isRunning
    maybeTV <- if isRunning'
                then consultSymbolTable symbolId
                else return Nothing

    value <- case maybeTV of
                Nothing -> return Nothing
                Just (_, v) -> return v

    return ([a], symbolId, t, value)

callStmt :: [Type] -> StateType ([Token], Maybe Type, Maybe Value)
callStmt symbolTypeList = do
    previousProgramState <- getProgramState
    (b, templateTypeList) <- option ([], []) templateInstantiation
    c <- TT.openParen
    (d, typeList, maybeValueList) <- unzip3 <$> option [] expStmtList
    e <- TT.closeParen

    openScope True -- TODO this cannot be True

    let OPEN_PAREN posn = c
    let runMethod templateIds paramList funcBody = do
            let (idList, expectedParamTypes) = unzip paramList
            assertValidParamList expectedParamTypes typeList posn
            valueList <- valueListFromMaybeValue maybeValueList posn
            instatiateArgs idList typeList valueList
            insertTemplateInstantiation templateTypeList templateIds
            runFuncBody funcBody
    let templateLen = genericLength templateTypeList
    maybeSymbolType <- searchTypeList symbolTypeList templateLen typeList
    symbolType <- case maybeSymbolType of 
        Nothing -> semanticError "no mathing function for call" -- TODO add symbolId to err msg
        Just t -> return t
    (maybeReturnT, maybeReturnV) <- case symbolType of
                            FuncType templateIds paramList returnT funcBody -> do
                                returnV <- runMethod templateIds paramList funcBody
                                case returnV of
                                    Nothing -> do
                                        warningMsg "function did not return a value"
                                        return (Just returnT, Nothing)
                                    Just returnedTV -> do
                                        value <- castValueToType returnT returnedTV posn
                                        return (Just returnT, Just value)
                            ProcType templateIds paramList funcBody -> do
                                returnedV <- runMethod templateIds paramList funcBody
                                when (isJust returnedV) $ semanticError
                                    ("procedure returned a value, but it shouldn't. Consider declaring as a func instead " ++ showPos posn)
                                return (Nothing, Nothing)
                            HaskellMethod _expectedTypeList returnT libMethod -> do
                                -- TODO assert expectedTypeList is equivalent to typeList
                                isRunning' <- isRunning
                                returnV <- if isRunning'
                                                then do
                                                    valueList <- valueListFromMaybeValue maybeValueList posn
                                                    libMethod valueList posn
                                                else return Nothing

                                return (returnT, returnV)
                            _ -> semanticError $ "Trying to call type " ++ show symbolType ++ ", it must be a function or procedure "  ++ showPos posn
    setProgramState previousProgramState
    closeScope
    return (b ++ [c] ++ concat d ++ [e], maybeReturnT, maybeReturnV)
    where
        valueListFromMaybeValue :: [Maybe Value] -> AlexPosn -> StateType [Value]
        valueListFromMaybeValue [] _ = return []
        valueListFromMaybeValue (h:t) posn = do
            case h of
                Nothing -> do
                    semanticError $ "trying to use unitialized var " ++ showPos posn
                Just v -> do
                    rest <- valueListFromMaybeValue t posn
                    return (v:rest)


        instatiateArgs :: [String] -> [Type] -> [Value] -> StateType ()
        instatiateArgs (idListHead:isListTail) (typeListHead:typeListTail) (valueListHead:valueListTail) = do
            insertSymbol (idListHead, typeListHead, Just valueListHead) False
            instatiateArgs isListTail typeListTail valueListTail
        instatiateArgs [] [] [] = return ()
        instatiateArgs [] _ _ = fail "<callStmt>"
        instatiateArgs _ [] _ = fail "<callStmt>"
        instatiateArgs _ _ [] = fail "<callStmt>"


        runFuncBody :: [Token] -> StateType (Maybe (Type, Value))
        runFuncBody funcBody = do
            previousProgramState <- getProgramState
            isRunning' <- isRunning
            if not isRunning' then
                return Nothing
                else do
                    st <- getState
                    parserResultFuncBody <- liftIO $ runParserT stmtList st "<callStmt>" funcBody
                    finalInterpreterState <- case parserResultFuncBody of
                                                Left _ -> fail "<callStmt>"
                                                Right (_, finalInterpreterState) -> return finalInterpreterState
                    setState finalInterpreterState
                    currProgramState <- getProgramState
                    maybeReturn <- case currProgramState of
                                        Return returnValue -> do
                                            return returnValue
                                        _ -> do
                                            return Nothing
                    setProgramState previousProgramState
                    return maybeReturn


literal :: StateType ([Token], Type, Maybe Value)
literal = do
    do
        a <- TT.intLiteral
        let INT_LITERAL _ v = a
        return ([a], IntType, Just $ IntValue v)
    <|> do
        a <- TT.floatLiteral
        let FLOAT_LITERAL _ v = a
        return ([a], FloatType, Just $ FloatValue v)
    <|> do
        a <- TT.stringLiteral
        let STRING_LITERAL _ v = a
        return ([a], StringType, Just $ StringValue v)
    <|> do
        a <- TT.kwTrue
        return ([a], BoolType, Just $ BoolValue True)
    <|> do
        a <- TT.kwFalse
        return ([a], BoolType, Just $ BoolValue False)
    <|> do
        a <- TT.openBracket
        let OPEN_BRACKET posn = a
        (bTokens, bt, bv) <- literal
        c <- many $ do
            d <- TT.kwComma
            (cTokens, ct, cv) <- literal
            assertTypesEq bt ct posn
            return (cTokens ++ [d], cv)
        d <- TT.closeBracket
        return ([a] ++ bTokens ++ concatMap fst c ++ [d], ArrayType bt, Just $ ArrayValue $ V.fromList (bv : map snd c))

expStmtList :: StateType [([Token], Type, Maybe Value)]
expStmtList = do
    a <- expStmt
    b <- many (do
            b <- TT.kwComma
            (cTokens, cT, cV) <- expStmt
            return (b:cTokens, cT, cV)
        )
    return (a:b)

baseExp :: StateType ([Token], Type, Maybe Value)
baseExp = do
    optionUnary <- optionMaybe (TT.opSub <|> TT.opNot)
    (base, baseT, baseV) <- literal
                            <|> (do
                                    (a, symbolId, typeList, varValue) <- var
                                    (do
                                        (b, maybeType, maybeValue) <- try $ callStmt typeList -- TODO remove this try
                                        expType <- case maybeType of
                                                        Nothing -> semanticError $ "called the procedure " ++ symbolId ++ " expecting a value"
                                                        Just t -> return t
                                        return (a ++ b, expType, maybeValue)) 
                                     <|> (do
                                            t <- getTypeFromTypeList typeList
                                            return (a, t, varValue))
                                )
                            <|> do
                                a <- TT.openParen
                                (b, expType, expValue) <- expStmt
                                c <- TT.closeParen
                                return ([a] ++ b ++ [c], expType, expValue)
                            <|> do
                                a <- TT.kwCast
                                b <- TT.opSmaller
                                (c, t) <- typeStmt
                                d <- TT.opGreater
                                e <- TT.openParen
                                (f, varId, varT, varV) <- var
                                g <- TT.closeParen
                                let OPEN_PAREN posn = e
                                varT' <- getTypeFromTypeList varT
                                finalT <- castType t varT' posn
                                isRunning' <- isRunning
                                finalV <- if isRunning' 
                                            then do
                                                case varV of
                                                    Nothing -> runtimeError $ "Using unitialized var \"" ++ varId ++ "\""
                                                    Just v -> do
                                                        finalV <- castValueToType finalT (varT', v) posn
                                                        return $ Just finalV
                                            else return Nothing
                                return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g], t, finalV)
                            <|> do
                                a <- TT.kwDeref
                                b <- TT.openParen
                                (c, varId, varType, varValue) <- var
                                let OPEN_PAREN posn = b
                                varType' <- getTypeFromTypeList varType
                                derefType <- case varType' of
                                                RefType derefType -> return derefType
                                                _ -> semanticError $ "Trying to deref a non reference type \"" ++ varId ++ "\"" ++ showPos posn
                                d <- TT.closeParen

                                isRunning' <- isRunning
                                searchRefResult <- if isRunning' 
                                            then do
                                                case varValue of
                                                    Just refValue -> do
                                                        case refValue of
                                                            RefValue refSymbol scopeId -> consultSymbolTableById (refSymbol, scopeId)
                                                            _ -> runtimeError "trying to deref a non reference value" -- TODO will not reach this
                                                    Nothing -> runtimeError $ "using unitialized var \"" ++ varId ++ "\""
                                            else return Nothing

                                finalV <- case searchRefResult of
                                    Nothing -> return Nothing
                                    Just (_refT, refV) -> return refV

                                return ([a] ++ [b] ++ c ++ [d], derefType, finalV)

    case optionUnary of
        Nothing -> return (base, baseT, baseV)
        Just unary -> do
            isRunning' <- isRunning
            if not isRunning' then do
                return (unary : base, baseT, Nothing)
            else do
                case unary of
                    OP_SUB posn -> do
                        (_, resultV) <- handleUnaryMinus baseV posn
                        return (unary : base, baseT, Just resultV)
                    OP_NOT posn -> do
                        (_, resultV) <- handleNot baseV posn
                        return (unary : base, baseT, Just resultV)
                    _ -> fail "<baseExp>" -- Will never reach this, just to avoid warnings


orExpStmt :: StateType ([Token], Type, Maybe Value)
orExpStmt = do
    (a, at, av) <- andExpStmt
    option (a, at, av) (do
            b <- TT.opOr
            let OP_OR posn = b
            (c, t, v) <- orExpStmt

            isRunning' <- isRunning
            if isRunning' then do
                (resultT, resultV) <- handleOr av v posn
                return (a ++ [b] ++ c, resultT, Just resultV)
            else do
                resultT <- resultOpType at t posn
                return (a ++ [b] ++ c, resultT, Nothing)
        )


andExpStmt :: StateType ([Token], Type, Maybe Value)
andExpStmt = do
    (a, at, av) <- compareExpStmt
    option (a, at, av) (do
            b <- TT.opAnd
            let OP_AND posn = b
            (c, t, v) <- andExpStmt

            isRunning' <- isRunning
            if isRunning' then do
                (resultT, resultV) <- handleAnd av v posn
                return (a ++ [b] ++ c, resultT, Just resultV)
            else do
                resultT <- resultOpType at t posn
                return (a ++ [b] ++ c, resultT, Nothing)
        )

compareExpStmt :: StateType ([Token], Type, Maybe Value)
compareExpStmt = do
    (a, at, av) <- addSubExpStmt
    option (a, at, av) (do
            (b, posn) <- do
                            b <- TT.opSmaller
                            let OP_SMALLER posn = b
                            return (b, posn)
                        <|> do
                            b <- TT.opSmallerEq
                            let OP_SMALLER_EQ posn = b
                            return (b, posn)
                        <|> do
                            b <- TT.opGreater
                            let OP_GREATER posn = b
                            return (b, posn)
                        <|> do
                            b <- TT.opGreaterEq
                            let OP_GREATER_EQ posn = b
                            return (b, posn)
                        <|> do
                            b <- TT.opEq
                            let OP_EQ posn = b
                            return (b, posn)
                        <|> do
                            b <- TT.opNotEq
                            let OP_NOT_EQ posn = b
                            return (b, posn)
            (c, t, v) <- addSubExpStmt

            isRunning' <- isRunning
            if isRunning' then do
                (resultT, resultV) <- handleComparison av v b posn
                return (a ++ [b] ++ c, resultT, Just resultV)
            else do
                resultT <- resultOpType at t posn
                return (a ++ [b] ++ c, resultT, Nothing)
        )


addSubExpStmt :: StateType ([Token], Type, Maybe Value)
addSubExpStmt = do
    (a, at, av) <- multDivExpStmt
    option (a, at, av) (do
            (b, posn, isAdd) <- do
                            b <- TT.opAdd
                            let OP_ADD posn = b
                            return (b, posn, True)
                        <|> do
                            b <- TT.opSub
                            let OP_SUB posn = b
                            return (b, posn, False)
            (c, t, v) <- addSubExpStmt

            isRunning' <- isRunning
            if isRunning' then do
                (resultT, resultV) <- if isAdd
                                        then handleAdd av v posn
                                        else handleSub av v posn
                return (a ++ [b] ++ c, resultT, Just resultV)
            else do
                resultT <- resultOpType at t posn
                return (a ++ [b] ++ c, resultT, Nothing)
        )

multDivExpStmt :: StateType ([Token], Type, Maybe Value)
multDivExpStmt = do
    (a, at, av) <- baseExp
    option (a, at, av) (do
            (b, posn, isMult) <- do
                            b <- TT.opMult
                            let OP_MULT posn = b
                            return (b, posn, True)
                        <|> do
                            b <- TT.opDiv
                            let OP_DIV posn = b
                            return (b, posn, False)
            (c, t, v) <- multDivExpStmt

            isRunning' <- isRunning
            if isRunning' then do
                (resultT, resultV) <- if isMult
                                        then handleMult av v posn
                                        else handleDiv av v posn
                return (a ++ [b] ++ c, resultT, Just resultV)
            else do
                resultT <- resultOpType at t posn
                return (a ++ [b] ++ c, resultT, Nothing)
        )


expStmt :: StateType ([Token], Type, Maybe Value)
expStmt = do
    orExpStmt

stmtList :: StateType ([Token], InterpreterState)
stmtList = do
    _ <- optionMaybe TT.newLine
    a <- stmt
    b <- concat <$> many (do
            b <- TT.newLine
            c <- option [] stmt
            return (b:c)
        )
    st <- getState
    return (a ++ b, st)

stmt :: StateType [Token]
stmt = do
    try varDecl -- TODO remove this try to improve errs
    <|> (do
        (a, symbolId, typeList, _varV) <- var
        b <- assignStmt symbolId typeList 
            <|> (do
                    (b, _, _) <- callStmt typeList
                    return b
                )
        return $ a ++ b)
    <|> ifElseStmt
    <|> whileStmt
    <|> forStmt
    <|> foreachStmt
    <|> do
        a <- TT.kwContinue
        let KW_CONTINUE posn = a
        assertContinuable posn
        isRunning' <- isRunning
        when isRunning' $ setProgramState Skip
        return [a]
    <|> do
        a <- TT.kwBreak
        let KW_BREAK posn = a
        assertBreakable posn
        isRunning' <- isRunning
        when isRunning' $ setProgramState Break
        return [a]
    <|> do
        a <- TT.kwReturn
        optionB <- optionMaybe expStmt
        b <- case optionB of
                Nothing -> do
                    let KW_RETURN posn = a
                    assertReturnType Nothing posn
                    isRunning' <- isRunning
                    when isRunning' $ setProgramState $ Return Nothing
                    return []
                Just (b, expType, expValue) -> do
                    let KW_RETURN posn = a
                    assertReturnType (Just expType) posn
                    isRunning' <- isRunning
                    when isRunning' $ do 
                        case expValue of
                            Nothing -> do
                                 -- Should not reach this, since this would be handled beforehand
                                semanticError $ "returning uintialized value " ++ showPos posn
                            Just v -> setProgramState $ Return $ Just (expType, v)
                    return b

        return $ a:b


mathOpSymbol :: StateType Token
mathOpSymbol = do
    TT.opAdd
    <|> TT.opSub
    <|> TT.opMult
    <|> TT.opDiv
    <|> TT.opAnd
    <|> TT.opOr
    <|> TT.opNot

assignStmt :: String -> [Type] -> StateType [Token]
assignStmt symbolId typeList = do
    optionB <- optionMaybe mathOpSymbol
    c <- TT.kwAssingment
    (d, expType, expValue) <- expStmt

    let KW_ASSIGNMENT posn = c
    symbolType <- getTypeFromTypeList typeList
    assertTypesEq symbolType expType posn

    isRunning' <- isRunning
    b <- case optionB of
            Nothing -> do
                when isRunning' $ do
                        case expValue of
                            Nothing -> semanticError $ "using uninitialized var " ++ showPos posn
                            Just v -> updateSymbolTable symbolId (typeList, Just v)
                return []
            Just op -> do
                when isRunning' $ do
                    maybeTV <- consultSymbolTable symbolId
                    maybeValue <- case maybeTV of
                                Nothing -> return Nothing
                                Just (_, v) -> return v
                    resultValue <- case op of
                        OP_ADD _ -> handleAdd maybeValue expValue posn
                        OP_SUB _ -> handleSub maybeValue expValue posn
                        OP_MULT _ -> handleMult maybeValue expValue posn
                        OP_DIV _ -> handleDiv maybeValue expValue posn
                        OP_AND _ -> handleAnd maybeValue expValue posn
                        OP_OR _ -> handleOr maybeValue expValue posn
                        _ -> semanticError $ "Invalid operation on assignment operation for " ++ symbolId ++ " " ++ showPos posn
                    castedValue <- castValueToType symbolType resultValue posn
                    updateSymbolTable symbolId ([symbolType], Just castedValue)
                    return ()

                return [op]

    return $ b ++ [c] ++ d

ifElseStmt :: StateType [Token]
ifElseStmt = do
    previousProgramState <- getProgramState
    previousParserBlock <- getParserBlock

    expectedReturnT <- getExpectedReturnT 
    setParserBlock $ Conditional expectedReturnT

    (a, ifExecuted) <- ifStmt
    if ifExecuted then
        setProgramState Skip
        else setProgramState previousProgramState
    b <- option [] elseIfElseRecursion

    setProgramState previousProgramState
    setParserBlock previousParserBlock
    return (a ++ b)
    where
        -- The Bool indicates whether the conditional was executed
        ifStmt :: StateType ([Token], Bool)
        ifStmt = do
            openScope True
            a <- TT.kwIf
            (b, expType, expValue) <- expStmt

            let KW_IF posn = a
            assertBooleanCompatible expType posn

            isRunning' <- isRunning
            condition <- if isRunning' then do
                                getBooleanValue expValue posn
                                else return False
            unless condition $ do setProgramState Skip

            c <- TT.kwColumn
            d <- TT.newLine
            e <- TT.indent
            (f, _) <- stmtList
            g <- TT.unindent

            closeScope
            return ([a] ++ b ++ [c] ++ [d] ++ [e] ++ f ++ [g], condition)

        elseIfElseRecursion :: StateType [Token]
        elseIfElseRecursion = do
            -- Only consume the newline if the next token is 'else'
            a <- try $ do
                a <- TT.newLine
                _ <- lookAhead TT.kwElse
                return a
            openScope True
            b <- TT.kwElse
            c <- do
                (c, executed) <- ifStmt
                when executed $ setProgramState Skip
                d <- option [] elseIfElseRecursion
                return $ c ++ d
                <|> (do
                    c <- TT.kwColumn
                    d <- TT.newLine
                    e <- TT.indent
                    (f, _) <- stmtList
                    g <- TT.unindent
                    return $ [c] ++ [d] ++ [e] ++ f ++ [g])

            closeScope
            return ([a] ++ [b] ++ c)


evaluateBooleanExp :: [Token] -> StateType (Maybe Value, InterpreterState)
evaluateBooleanExp tokenList = do
    previusState <- getState

    parserExpResult <- liftIO $ runParserT expStmt previusState "<evaluateBooleanExp>" tokenList
    expValue <- case parserExpResult of
                        Left _ -> fail "<evaluateBooleanExp>"
                        Right (_, _, expValue) -> return expValue
    resultState <- getState
    return (expValue, resultState)

endLoopEarly :: StateType Bool
endLoopEarly = do
    currProgramState <- getProgramState
    return (currProgramState == Break || currProgramState == Return {})

whileStmt :: StateType [Token]
whileStmt = do
    previousProgramState <- getProgramState
    previousParserBlock <- getParserBlock
    openScope True

    a <- TT.kwWhile
    expectedReturnT <- getExpectedReturnT 
    setParserBlock $ Loop expectedReturnT
    (b, expType, expValue) <- expStmt

    let KW_WHILE posn = a

    assertBooleanCompatible expType posn
    isRunning' <- isRunning
    condition <- if isRunning' then do
                        getBooleanValue expValue posn
                        else return False
    unless condition $ setProgramState Skip

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    (f, _) <- stmtList
    g <- TT.unindent

    let runWhile = do
            endEarly <- endLoopEarly
            unless endEarly $ do
                setProgramState previousProgramState

                (expValue', resultState) <- evaluateBooleanExp b
                condition' <- getBooleanValue expValue' posn
                setState resultState

                when condition' $ do
                    st <- getState
                    parserResult <- liftIO $ runParserT stmtList st "<while>" f
                    case parserResult of
                        Left _ -> fail "<while>"
                        Right (_, resultState') -> putState resultState'
                    runWhile

    when condition runWhile
    setProgramState previousProgramState
    setParserBlock previousParserBlock
    closeScope

    return ([a] ++ b ++ [c] ++ [d] ++ [e] ++ f ++ [g])

typeStmt :: StateType ([Token], Type)
typeStmt = do
    a <- optionMaybe constDecl
    (b, t) <- do
                b <- TT.kwInt
                return ([b], IntType)
            <|> do
                b <- TT.kwFloat
                return ([b], FloatType)
            <|> do
                b <- TT.kwBool
                return ([b], BoolType)
            <|> do
                b <- TT.kwString
                return ([b], StringType)
            <|> do -- refType
                b <- TT.kwRef
                c <- TT.openParen
                (d, t) <- typeStmt
                e <- TT.closeParen
                return ([b] ++ [c] ++ d ++ [e], RefType t)
            <|> do -- customType
                b <- TT.id
                let ID posn s = b
                maybeT <- consultTypeList s posn >>= getEnumOrStructTypes
                case maybeT of
                    Nothing -> semanticError $ s ++ " is not a type " ++ showPos posn
                    Just t -> return ([b], t)
            <|> do -- reference for method
                b <- TT.openParen
                (c, templateIds) <- option ([], []) templateDecl
                d <- TT.openParen
                (e, paramList) <- optParamDeclList
                f <- TT.closeParen
                optionG <- optionMaybe returnDecl

                let (gTokens, t) = case optionG of
                        Nothing -> ([], ProcRefType templateIds (map snd paramList))
                        Just (returnTokens, returnType) -> (returnTokens, FuncRefType templateIds (map snd paramList) returnType)

                return ([b] ++ c ++ [d] ++ e ++ [f] ++ gTokens, t)
    maybeC <- optionMaybe arrayDecl

    (aTokens, afterConst) <- case a of
                    Nothing -> return ([], t)
                    Just aTokens -> return ([aTokens], ConstType t)
    (cTokens, finalType) <- case maybeC of
                                Nothing -> return ([], afterConst)
                                Just c -> return (c, ArrayType afterConst)

    return (aTokens ++ b ++ cTokens, finalType)
    where constDecl = TT.kwConst

forStmt :: StateType [Token]
forStmt = do
    previousParserBlock <- getParserBlock
    previousProgramState <- getProgramState
    openScope True
    a <- TT.kwFor
    expectedReturnT <- getExpectedReturnT 
    setParserBlock $ Loop expectedReturnT
    b <- option [] varDecl
    c <- TT.kwSemicolumn
    optionD <- optionMaybe expStmt

    let KW_SEMICOLUMN posn = c

    (d, expType, expValue) <- case optionD of
                Nothing -> return ([KW_TRUE posn], BoolType, Just $ BoolValue True) -- if condition is empty True will be used
                Just (d, dType, dValue) -> return (d, dType, dValue)

    assertBooleanCompatible expType posn

    e <- TT.kwSemicolumn
    setProgramState Skip -- Don't execute assignStmt yet no mater what
    optionF <- optionMaybe (do
                                (f, _) <- loopIncrementStmt
                                return f
                            )
    let f = fromMaybe [] optionF
    setProgramState previousProgramState

    isRunning' <- isRunning
    condition <- if isRunning' then do
                        getBooleanValue expValue posn
                        else return False
    unless condition $ setProgramState Skip

    g <- TT.kwColumn
    h <- TT.newLine
    i <- TT.indent
    (j, _) <- stmtList
    k <- TT.unindent

    let runFor = do
            endEarly <- endLoopEarly
            unless endEarly $ do
                setProgramState previousProgramState

                -- Perform operation to be performed after loop
                st <- getState
                parserResultAssingStmt <- liftIO $ runParserT loopIncrementStmt st "<for>" f
                case parserResultAssingStmt of
                    Left _ -> fail "<for>"
                    Right (_, resultState) -> putState resultState

                (expValue', resultState) <- evaluateBooleanExp d
                condition' <- getBooleanValue expValue' posn
                setState resultState

                when condition' $ do
                    st' <- getState
                    parserResult <- liftIO $ runParserT stmtList st' "<for>" j
                    case parserResult of
                        Left _ -> fail "<for>"
                        Right (_, resultState') -> putState resultState'

                    runFor

    when condition runFor
    setProgramState previousProgramState
    setParserBlock previousParserBlock
    closeScope

    return ([a] ++ b  ++ [c] ++ d ++ [e] ++ f ++ [g] ++ [h] ++ [i] ++ j ++ [k])
    where 
        loopIncrementStmt :: StateType ([Token], InterpreterState)
        loopIncrementStmt = do
                                (f, symbolId, typeList, _varV) <- var 
                                g <- assignStmt symbolId typeList 
                                    <|> (do 
                                            (g, _, _) <- callStmt typeList
                                            return g
                                        )
                                    <?> "loop increment should be either an assignStmt or a method call"
                                finalState <- getState
                                return (f ++ g, finalState)

        

foreachStmt :: StateType [Token]
foreachStmt = do
    previousParserBlock <- getParserBlock
    openScope True
    a <- TT.kwForeach
    expectedReturnT <- getExpectedReturnT 
    setParserBlock $ Loop expectedReturnT
    b <- TT.id
    c <- TT.kwIn
    d <- TT.id

    let ID posn dSymbol = d
    dType <- consultType dSymbol posn
    assertIterableType dSymbol dType posn

    let ArrayType underlyingType = dType
    let ID _ bSymbol = b
    insertSymbol (bSymbol, underlyingType, Nothing) False

    e <- TT.kwColumn
    f <- TT.newLine
    g <- TT.indent
    (h, _) <- stmtList
    i <- TT.unindent

    closeScope
    setParserBlock previousParserBlock
    return $ [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] ++ h ++ [i]

parser :: [Token] -> IO (Either ParseError [Token])
parser tokenList = do
    interpreterState <- initInterpreterState
    parserResult <- runParserT vectraLanguage interpreterState "Error message" tokenList
    case parserResult of
        Left a -> return $ Left a
        Right (t, finalState) -> do
            let finalProgramState = programState finalState
            case finalProgramState of
                Finished -> return $ Right t
                _ -> do
                    putStrLn "Error: no main method"
                    return $ Right [] -- TODO should be Left
