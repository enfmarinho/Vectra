{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Parser
  ( parser
  ) where

import InterpreterState
import qualified TerminalTokens as TT
import Utils
import Data.List.Split (splitOn)
import Text.Parsec.Error (newErrorMessage, Message(..))
import Text.Parsec.Pos   (initialPos)
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
import Data.List (intercalate, genericLength)

-- TODO add file name to messages

importFile :: String -> AlexPosn -> StateType ()
importFile filePath _posn = do
    result <- searchImport filePath
    case result of
        Nothing -> do
            addImport filePath
            currState <- getState

            fileTokens <- liftIO $ getTokens filePath
            importTokens <- case fileTokens of
                                Left _ -> fail $ "syntax failure on file " ++ filePath
                                Right t -> return t

            parserResult <- liftIO $ runParserT vectraLanguage currState "<import>" importTokens
            case parserResult of
                Left _ -> fail $ "semantic failure on file " ++ filePath
                Right finalState -> do
                    putState finalState
                    finishImport filePath
        Just b -> unless b $ semanticError $ "cyclic importing " ++ filePath


vectraLanguage :: StateType InterpreterState
vectraLanguage = do
    _ <- importCommand `sepEndBy` TT.newLine
    _ <- globalDecl `sepEndBy` TT.newLine
    getState

    where
        importCommand :: StateType ()
        importCommand = do
            _ <- TT.kwImport
            _ <- importList `sepEndBy1` TT.kwComma
            return ()
            where importList = do
                    a <- TT.id
                    let ID posn symbolId = a
                    importSpecialMethod symbolId posn
                    <|> do
                        a <- TT.stringLiteral
                        let STRING_LITERAL posn filePath = a
                        importFile filePath posn

        globalDecl :: StateType ()
        globalDecl = do
            structDecl
            <|> implDecl
            <|> enumDecl
            <|> subprogramDecl
            <|> do
                _ <- varDecl
                return ()
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

    let cTokens = concatMap fst c
    let cIds = map snd c

    return ([a] ++ [b] ++ cTokens ++ [d], bId : cIds)
  where
    idSymbol :: StateType (Token, String)
    idSymbol = do
        a@(ID posn symbolId) <- TT.id
        insertSymbol (symbolId, TemplateType $ Just symbolId, Nothing) posn
        return (a, symbolId)

templateInstantiation :: StateType ([Token], [Type])
templateInstantiation = do
    a <- TT.opSmaller
    (b, bType) <- typeStmt
    rest <- many $ do
        c <- TT.kwComma
        (d, dType) <- typeStmt
        return (c:d, dType)
    let tokenList = b ++ concatMap fst rest
        typeList = bType : map snd rest
    c <- TT.opGreater
    return ([a] ++ tokenList ++ [c], typeList)

insertTemplateInstantiation :: [Type] -> [String] -> AlexPosn -> StateType ()
insertTemplateInstantiation [] [] _= return ()
insertTemplateInstantiation (_:_) [] posn = semanticError $ "Template instantiation: missing symbols at " ++ showPos posn
insertTemplateInstantiation [] (_:_) posn = semanticError $ "Template instantiation: missing types" ++ showPos posn
insertTemplateInstantiation (t:typeRest) (s:symbolRest) posn = do
    insertSymbol (s, t, Nothing) posn
    insertTemplateInstantiation typeRest symbolRest posn

structDecl :: StateType ()
structDecl = do
    openScope True
    _ <- TT.kwStruct
    (_, templateIds) <- option ([], []) templateDecl
    (ID posn symbolId) <- TT.id

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
                    _ <- TT.newLine
                    currScope <- topScope
                    closeScope

                    case accessModifier of
                        Public -> liftIO $ mergeTablesInPlace publicTable currScope
                        Private -> liftIO $ mergeTablesInPlace privateTable currScope
                        Static -> return () -- Cannot reach this, since we don't allow static data, just to avoid warnings
                    )
    _ <- TT.unindent
    closeScope
    insertSymbol (symbolId, StructType symbolId templateIds publicTable privateTable, Nothing) posn

implDecl :: StateType ()
implDecl = do
    openScope True
    _ <- TT.kwImpl
    (ID posn symbolId) <- TT.id
    (typeList, _) <- consultSymbol symbolId posn
    structT <- getTypeFromTypeList typeList
    case structT of
        StructType _name templeteList publicTable privateTable -> do
            pushScope publicTable True
            pushScope privateTable True
            insertTemplates templeteList posn
        _ -> semanticError $ "using impl for a non-struct type \"" ++ symbolId ++ "\" " ++ showPos posn

    implMaybe <- consultSymbolMaybe ("impl::" ++ symbolId) 
    (publicMethodTable, privateMethodTable) <- case implMaybe of
                                                    Nothing -> do
                                                        emptyTable <- liftIO H.new
                                                        return (emptyTable, emptyTable)
                                                    Just (implT, _) -> case implT of
                                                                            [ImplType pub priv] -> return (pub, priv)
                                                                            _ -> semanticError "<implDecl>"


    pushScope publicMethodTable True
    pushScope privateMethodTable True
    
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

                            let isStatic = f /= Static
                            when (f == Static) $ do
                                pushNamespacePrefix symbolId
                            openScope isStatic -- open temporary scope 
                            _ <- subprogramDecl
                            _ <- optionMaybe TT.newLine
                            currScope <- topScope
                            closeScope -- close temporary scope 
                            when (f == Static) $ do
                                popNamespacePrefix

                            case f of
                                Public -> liftIO $ mergeTablesInPlace currScope publicMethodTable
                                Private -> liftIO $ mergeTablesInPlace currScope privateMethodTable
                                Static -> mergeTableToGlobal currScope
                            return []
                        )
    _ <- TT.unindent

    closeScope -- closing scope for private methods
    closeScope -- closing scope for public methods
    closeScope -- closing scope for private data 
    closeScope -- closing scope for public data

    when (isNothing implMaybe) $
        insertSymbol ("impl" ++ symbolId, ImplType publicMethodTable privateMethodTable, Nothing) posn

    where
        insertTemplates :: [String] -> AlexPosn -> StateType ()
        insertTemplates [] _ = return ()
        insertTemplates (h:t) posn = do
            insertSymbol (h, TemplateType $ Just h, Nothing) posn
            insertTemplates t posn

namespaceDecl :: StateType ()
namespaceDecl = do
    _ <- TT.kwNamespace
    (ID posn symbolId) <- TT.id
    assertNonAmbiguous symbolId posn 
    pushNamespacePrefix symbolId
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    _ <- concat <$> many1 (do
                            _ <- do
                                    _ <- varDecl
                                    return ()
                                <|> subprogramDecl
                                <|> enumDecl
                                <|> structDecl
                                <|> implDecl
                                <|> namespaceDecl
                            _ <- optionMaybe TT.newLine
                            return []
                        )
    _ <- TT.unindent
    popNamespacePrefix

enumDecl :: StateType ()
enumDecl = do
    _ <- TT.kwEnum
    (ID posn enumId) <- TT.id
    assertNonAmbiguous enumId posn

    pushNamespacePrefix enumId

    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    _ <- idList enumId posn
    _ <- TT.unindent

    popNamespacePrefix
    insertSymbol (enumId, EnumLabelType enumId, Nothing) posn
    where
        idList :: String -> AlexPosn -> StateType ()
        idList enumId posn = do
            _ <- many1 $ do
                (ID _posn labelId) <- TT.id
                _ <- TT.newLine
                insertSymbol (labelId, EnumLabelType enumId, Just $ EnumValue labelId) posn
            return ()

optUnnamedParamDeclList :: StateType ([Token], [Type])
optUnnamedParamDeclList = do
    option ([], []) unnamedParamDeclList
    where
        unnamedParamDeclList :: StateType ([Token], [Type])
        unnamedParamDeclList = do
            (a, aT) <- typeStmt
            rest <- many $ do
                b <- TT.kwComma
                (c, cT) <- typeStmt
                return (b:c, cT)
            let tokenList = a ++ concatMap fst rest
                paramList = aT : map snd rest
            return (tokenList, paramList)

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
            b@(ID posn symbolId) <- TT.id
            insertSymbol (symbolId, varType, Nothing) posn
            return (a ++ [b], (symbolId, varType))

subprogramDecl :: StateType ()
subprogramDecl = do
    previousParserBlock <- getParserBlock
    openScope True
    a <- TT.kwProc
        <|> TT.kwFunc
    (_, bIds) <- option ([], []) templateDecl
    (ID posn symbolId) <- TT.id

    _ <- TT.openParen
    (_, dParams) <- optParamDeclList
    _ <- TT.closeParen
    optionF <- optionMaybe returnDecl

    case optionF of
        Nothing -> setParserBlock (Method Nothing)
        Just (_, t) -> setParserBlock (Method $ Just t)

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

            insertSymbol (symbolId, ProcType bIds dParams g, Nothing) posn
        KW_FUNC _ -> case optionF of
                        Nothing -> semanticError $ "A function must return something. Consider declaring "
                                                    ++ symbolId ++ " as a procedure instead " ++ showPos posn
                        Just (_, returnType) -> insertSymbol (symbolId, FuncType bIds dParams returnType g, Nothing) posn
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

arrayDecl :: Type -> StateType ([Token], Type, Maybe Value)
arrayDecl underlyingT = do
    a@(OPEN_BRACKET posn) <- TT.openBracket
    maybeB <- optionMaybe expStmt
    c <- TT.closeBracket
    maybeD <- optionMaybe (arrayDecl $ ArrayType underlyingT)

    (b, size) <- 
        case maybeB of
            Nothing -> return ([], 1)
            Just (b, expT, expV) -> do
                assertNumberType expT posn
                size <- getIntValue expV posn
                return (b, size)


    (d, finalT, finalV) <- case maybeD of
                            Nothing -> do
                                return ([], ArrayType underlyingT, Just $ ArrayValue $ V.replicate size Nothing)
                            Just (d, t, v) -> return (d, ArrayType t, Just $ ArrayValue $ V.replicate size v)
                        
    return ([a] ++ b ++ [c] ++ d, finalT, finalV)

    where
        assertNumberType :: Type -> AlexPosn -> StateType ()
        assertNumberType value posn = do
            case value of
                IntType -> return ()
                _ -> semanticError $ "Array size should be either empty or a int type " ++ showPos posn

        getIntValue :: Maybe Value -> AlexPosn -> StateType Int
        getIntValue (Just (IntValue v)) _ = return v
        getIntValue _ posn = semanticError $ "Array size should be either empty or a int type " ++ showPos posn

returnDecl :: StateType ([Token], Type)
returnDecl = do
    _ <- TT.opSub
    _ <- TT.opGreater
    (a, aType) <- typeStmt
    optionB <- optionMaybe (arrayDecl aType)
    (b, returnType) <- case optionB of
        Nothing -> return ([], aType)
        Just (b, t, _) -> return (b, t)
    return (a ++ b, returnType)

-- TODO make arrayDecl recursive to allow multiple dimension arrays
varDecl :: StateType [Token]
varDecl = do
    (b, bType) <- typeStmt
    c@(ID posn symbolId) <- TT.id
    optionD <- optionMaybe (arrayDecl bType)
    (d, varType) <- case optionD of
                        Nothing -> return ([], bType)
                        Just (d, t, _) -> return (d, t)
    insertSymbol (symbolId, varType, Nothing) posn
    e <- do
            e <- TT.kwAssingment
            (f, expType, maybeExpValue) <- expStmt
            assertTypesEq bType expType posn
            isRunning' <- isRunning
            when isRunning' $ do
                case maybeExpValue of
                    Nothing -> runtimeError $ "trying to use unitialized variable " ++ showPos posn
                    Just v -> do
                        finalValue <- castValueToType varType (expType, v) posn
                        updateSymbol symbolId ([varType], Just finalValue)
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
    (b, symbolId, posn) <- namespaceAccess
    -- (b, symbolList) <- option ([], []) memberAccess

    (varTypeList, varValue) <- consultSymbol symbolId posn
    return (b, symbolId, varTypeList, varValue)

callStmt :: String -> [Type] -> StateType ([Token], Maybe Type, Maybe Value)
callStmt symbolId symbolTypeList = do
    previousNamespaceStack <- getNamespaceStack
    let namespaceList = removeLast symbolId
    pushMultipleNamespacePrefix namespaceList
    openScope True -- True because it needs access to the param values, it will be changed to false latter on
    -- TODO read a optional dot followed by var, to allow method calls
    previousProgramState <- getProgramState
    (b, templateTypeList) <- option ([], []) templateInstantiation
    c <- TT.openParen
    (d, typeList, maybeValueList) <- unzip3 <$> option [] expStmtList
    e <- TT.closeParen

    let OPEN_PAREN posn = c
    let runMethod templateIds paramList funcBody = do
            let (idList, expectedParamTypes) = unzip paramList
            assertValidParamList expectedParamTypes typeList posn -- TODO check this for correctness
            insertTemplateInstantiation templateTypeList templateIds posn
            instatiateArgs idList expectedParamTypes (typeList, maybeValueList) posn
            changeTopScopeVisibility False
            runFuncBody funcBody
    let templateLen = genericLength templateTypeList
    maybeSymbolType <- searchTypeList symbolTypeList templateLen typeList
    symbolType <- case maybeSymbolType of
        Nothing -> semanticError $ "no matching function to call \"" ++ symbolId ++ "\"" ++ showPos posn
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
                            HaskellMethod expectedTypeList returnT libMethod -> do
                                assertValidParamList expectedTypeList typeList posn
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
    setNamespaceStack previousNamespaceStack
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

        removeLast :: String -> String
        removeLast s =
            case splitOn "::" s of
                [_] -> s  -- no "::" found
                parts -> intercalate "::" (init parts)


        instatiateArgs :: [String] -> [Type] -> ([Type], [Maybe Value]) -> AlexPosn -> StateType ()
        instatiateArgs (currId:idsTail) (expectedType:expectedTypesTail) (currType:typesTail, currValue:valuesTail) posn = do
            isRunning' <- isRunning
            finalV <- if isRunning'
                        then do
                            case currValue of
                                Nothing -> runtimeError $ "calling subprogram with unitialized var " ++ currId ++ showPos posn
                                Just v -> do
                                    expectedType' <- case expectedType of
                                                        TemplateType s -> do
                                                            (t, _) <- case s of
                                                                        Nothing -> semanticError $ "missing template instatiation " ++ showPos posn -- cannot reach this
                                                                        Just s' -> do
                                                                            result <- consultSymbolMaybe s'
                                                                            case result of
                                                                                Nothing -> semanticError $ "cannot find template instatiation " ++ showPos posn -- cannot reach this
                                                                                Just t -> return t
                                                            getTypeFromTypeList t
                                                        _ -> return expectedType

                                    finalV <- castValueToType expectedType' (currType, v) posn
                                    return $ Just finalV
                        else return Nothing
            insertSymbol (currId, expectedType, finalV) posn
            instatiateArgs idsTail expectedTypesTail (typesTail, valuesTail) posn
        instatiateArgs [] [] ([], []) _ = return ()
        instatiateArgs [] _ (_, _) _ = fail "<callStmt>"
        instatiateArgs _ [] (_, _) _ = fail "<callStmt>"
        instatiateArgs _ _ ([], _) _ = fail "<callStmt>"
        instatiateArgs _ _ (_, []) _ = fail "<callStmt>"

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

namespaceAccess :: StateType ([Token], String, AlexPosn)
namespaceAccess = do
    a@(ID posn symbolId) <- TT.id
    segments <- many $ do
        access <- TT.kwDoubleColumn
        name@(ID _ nameId) <- TT.id
        return (access : [name], nameId)

    let (tokenList, idList) = unzip segments

    return (a : concat tokenList, intercalate "::" (symbolId : idList) , posn)

-- memberAccess :: StateType ([Token], [String])
-- memberAccess = do
--     segments <- many $ do
--         dot <- TT.kwDot
--         c@(ID _ cid) <- TT.id
--         return ([dot, c], cid)
--
--     let (tokenList, idList) = unzip segments
--
--     return (concat tokenList, idList)

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
    <|> try (do
        a <- TT.openBracket
        let OPEN_BRACKET posn = a
        (bTokens, bt, bv) <- literal
        c <- many $ do
            d <- TT.kwComma
            (cTokens, ct, cv) <- literal
            assertTypesEq bt ct posn
            return (cTokens ++ [d], cv)
        d <- TT.closeBracket
        return ([a] ++ bTokens ++ concatMap fst c ++ [d], ArrayType bt, Just $ ArrayValue $ V.fromList (bv : map snd c)))
    <|> try (do
        (a, at) <- typeStmt
        b <- TT.openBracket
        c <- TT.intLiteral
        d <- TT.closeBracket
        let INT_LITERAL _ size = c
        return (a ++ [b] ++ [c] ++ [d], ArrayType at, Just $ ArrayValue $ V.replicate size Nothing))
    <|> try (do -- enum labels
        (b, symbolId, posn) <- namespaceAccess
        (tList, v) <- consultSymbol symbolId posn
        t <- getTypeFromTypeList tList
        case t of
            EnumLabelType _ -> return ()
            _ -> semanticError $ "should be a enum label " ++ showPos posn
        return (b, t, v)
        )
    <|> try (do
            (a, structSymbolId, posn) <- namespaceAccess
            (b, templateTypeList) <- option ([], []) templateInstantiation
            c <- TT.openCurly
            d <- TT.newLine
            e <- TT.indent

            (structTList, _) <- consultSymbol structSymbolId posn
            structT <- getTypeFromTypeList structTList
            openScope True
            (publicTable, privateTable) <- case structT of
                    StructType _ templateList publicTable privateTable -> do
                        insertTemplateInstantiation templateTypeList templateList posn
                        cpub <- liftIO $ copyTable publicTable
                        cpriv <- liftIO $ copyTable privateTable
                        return (cpub, cpriv)
                    _ -> semanticError $ "special initialization should be only used for struct types " ++ showPos posn

            f <- many1 $ do
                f@(ID posn' symbolId) <- TT.id
                g <- TT.kwAssingment
                (h, expT, expV) <- expStmt
                i <- TT.newLine

                publicSearch <- liftIO $ H.lookup publicTable symbolId
                case publicSearch of
                    Nothing -> do
                        privateSearch <- liftIO $ H.lookup privateTable symbolId
                        case privateSearch of 
                            Nothing -> semanticError $ 
                                "no symbol \"" ++ symbolId ++ "\" in struct \"" ++ structSymbolId ++ "\" " ++ showPos posn'
                            Just (tList, _) -> do
                                t <- getTypeFromTypeList tList
                                liftIO $ H.delete privateTable symbolId
                                assertTypesEq t expT posn
                                insertSymbol (symbolId, t, expV) posn'
                    Just (tList, _) -> do
                            t <- getTypeFromTypeList tList
                            liftIO $ H.delete publicTable symbolId
                            assertTypesEq t expT posn
                            insertSymbol (symbolId, t, expV) posn'
                        

                return ([f] ++ [g] ++ h ++ [i])

            privateMissingInit <- liftIO $ H.toList privateTable
            publicMissingInit <- liftIO $ H.toList publicTable
            let missingSymbols = publicMissingInit ++ privateMissingInit
            case missingSymbols of
                [] -> return ()
                _ -> semanticError $ "symbols missing initialization, such as: " ++ show missingSymbols ++ " at " ++ showPos posn
            let unzipedF = concat f
            g <- TT.unindent
            h <- TT.newLine
            i <- TT.closeCurly

            currTable <- topScope
            closeScope
            return (a ++ b ++ [c] ++ [d] ++ [e] ++ unzipedF ++ [g] ++ [h] ++ [i], StructInstanceType structSymbolId, Just $ StructValue currTable)
        )

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
    (base, baseT, baseV) <- try literal
                            <|> (do
                                    (a, symbolId, typeList, varValue) <- var
                                    (do
                                        (b, maybeType, maybeValue) <- try $ callStmt symbolId typeList -- TODO remove this try
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
                                e@(OPEN_PAREN posn) <- TT.openParen
                                (f, varT, varV) <- expStmt
                                g <- TT.closeParen
                                finalT <- castType t varT posn
                                isRunning' <- isRunning
                                finalV <- if isRunning'
                                            then do
                                                case varV of
                                                    Nothing -> runtimeError $ "Using unitialized var " ++ showPos posn
                                                    Just v -> do
                                                        finalV <- castValueToType finalT (varT, v) posn
                                                        return $ Just finalV
                                            else return Nothing
                                return ([a] ++ [b] ++ c ++ [d] ++ [e] ++ f ++ [g], t, finalV)
                            <|> do
                                a <- TT.kwDeref
                                b@(OPEN_PAREN posn) <- TT.openParen
                                (c, varId, varType, varValue) <- var
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
                                                            RefValue refSymbol scopeId -> consultSymbolByIdMaybe (refSymbol, scopeId)
                                                            _ -> runtimeError "trying to deref a non reference value" -- TODO will not reach this
                                                    Nothing -> runtimeError $ "using unitialized var \"" ++ varId ++ "\""
                                            else return Nothing

                                finalV <- case searchRefResult of
                                    Nothing -> return Nothing
                                    Just (_refT, refV) -> return refV

                                return ([a] ++ [b] ++ c ++ [d], derefType, finalV)
                            <|> do
                                a <- TT.kwRef
                                b@(OPEN_PAREN posn) <- TT.openParen
                                (c, symbolId, _symbolT, _symbolV) <- var
                                d <- TT.closeParen
                                (refT, maybeRefV) <- getSymbolRef symbolId posn
                                return ([a] ++ [b] ++ c ++ [d], refT, maybeRefV)

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
                assertComparableTypes at t posn
                return (a ++ [b] ++ c, BoolType, Nothing)
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
    <|> derefAssignStmt
    <|> (do
        (a, symbolId, typeList, _varV) <- var
        b <- assignStmt symbolId typeList
            <|> (do
                    (b, _, _) <- callStmt symbolId typeList
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
        when isRunning' $ setProgramState Continue
        return [a]
    <|> do
        a <- TT.kwBreak
        let KW_BREAK posn = a
        assertBreakable posn
        isRunning' <- isRunning
        when isRunning' $ setProgramState Break
        return [a]
    <|> do
        a@(KW_RETURN posn) <- TT.kwReturn
        optionB <- optionMaybe expStmt
        b <- case optionB of
                Nothing -> do
                    assertReturnType Nothing posn
                    isRunning' <- isRunning
                    when isRunning' $ setProgramState $ Return Nothing
                    return []
                Just (b, expType, expValue) -> do
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

derefAssignStmt :: StateType [Token]
derefAssignStmt = do
    a <- TT.kwDeref
    b@(OPEN_PAREN varPosn) <- TT.openParen
    (c, symbolId, varT, varV) <- var -- TODO maybe <|> callStmt with "try"

    underlyingT <- case varT of
                    [RefType t] -> return t
                    _ -> semanticError $ "Trying to deref a non ref type at " ++ showPos varPosn

    d <- TT.closeParen
    optionE <- optionMaybe mathOpSymbol
    f@(KW_ASSIGNMENT posn) <- TT.kwAssingment
    (g, expType, expValue) <- expStmt

    assertTypesEq underlyingT expType posn

    isRunning' <- isRunning
    e <- if not isRunning' 
            then return []
            else do
                case optionE of
                    Nothing -> do
                        case varV of
                            Just v -> case v of
                                        RefValue referencedId referencedTableId -> 
                                            updateSymbolById (referencedId, referencedTableId) ([underlyingT], expValue)
                                        _ -> semanticError $ "trying to deref a non ref value at " ++ showPos posn -- will not reach this
                            Nothing -> semanticError $ "Trying to deref a null ref \"" ++ symbolId ++ "\" at " ++ showPos posn
                        return []
                    Just op -> do
                            (referencedId, referencedTableId) <- 
                                case varV of
                                    Nothing -> semanticError $ "2trying to assign by reference to a non-reference type at " ++ showPos varPosn
                                    Just v -> case v of
                                                RefValue rid rtid -> return (rid, rtid)
                                                _ -> semanticError ""

                            
                            (_, maybeValue) <- consultSymbolById (referencedId, referencedTableId) varPosn
                            resultValue <- case op of
                                OP_ADD _ -> handleAdd maybeValue expValue posn
                                OP_SUB _ -> handleSub maybeValue expValue posn
                                OP_MULT _ -> handleMult maybeValue expValue posn
                                OP_DIV _ -> handleDiv maybeValue expValue posn
                                OP_AND _ -> handleAnd maybeValue expValue posn
                                OP_OR _ -> handleOr maybeValue expValue posn
                                _ -> semanticError $ "Invalid operation on assignment operation for " ++ symbolId ++ " " ++ showPos posn
                            castedValue <- castValueToType underlyingT resultValue posn
                            updateSymbolById (referencedId, referencedTableId) ([underlyingT], Just castedValue)
                            return [op]

    return $ [a] ++ [b] ++ c ++ [d] ++ e ++ [f] ++ g

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
                            Just v -> updateSymbol symbolId (typeList, Just v)
                return []
            Just op -> do
                when isRunning' $ do
                    maybeTV <- consultSymbolMaybe symbolId
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
                    updateSymbol symbolId ([symbolType], Just castedValue)
                    return ()

                return [op]

    return $ b ++ [c] ++ d

ifElseStmt :: StateType [Token]
ifElseStmt = do
    (a, _) <- ifStmt
    b <- option [] elseIfElseRecursion

    currProgramState <- getProgramState
    when (currProgramState == Skip) (setProgramState Running)
    return (a ++ b)
    where
        -- The Bool indicates whether the conditional was executed
        ifStmt :: StateType ([Token], Bool)
        ifStmt = do
            previousProgramState <- getProgramState
            openScope True
            a@(KW_IF posn) <- TT.kwIf
            (b, expType, expValue) <- expStmt

            assertBooleanCompatible expType posn

            isRunning' <- isRunning
            executed <- if isRunning' then do
                                condition <- getBooleanValue expValue posn
                                unless condition $ do setProgramState Skip
                                return condition
                                else return False

            c <- TT.kwColumn
            d <- TT.newLine
            e <- TT.indent
            (f, _) <- stmtList
            g <- TT.unindent

            closeScope
            currProgramState <- getProgramState
            when (executed && currProgramState == Running) $ do
                    setProgramState Skip

            when (currProgramState == Skip) $ do
                setProgramState previousProgramState
            return ([a] ++ b ++ [c] ++ [d] ++ [e] ++ f ++ [g], executed)

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
                (c, _) <- ifStmt
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

    a@(KW_WHILE posn) <- TT.kwWhile
    expectedReturnT <- getExpectedReturnT
    setParserBlock $ Loop expectedReturnT
    (b, expType, expValue) <- expStmt

    assertBooleanCompatible expType posn
    isRunning' <- isRunning
    condition <- if isRunning' then do
                        condition <- getBooleanValue expValue posn
                        unless condition (setProgramState Skip)
                        return condition
                        else return False

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    (f, _) <- stmtList
    g <- TT.unindent

    let runWhile = do
            closeScope
            openScope True
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
    currProgramState <- getProgramState
    case currProgramState of
        Return {} -> return ()
        _ -> do
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
                (c, symbolId, posn) <- namespaceAccess
                (tList, _) <- consultSymbol symbolId posn
                t <- getTypeFromTypeList tList
                assertCustomType t posn
                return (c, t)
    maybeC <- optionMaybe (arrayDecl t)

    (aTokens, afterConst) <- case a of
                    Nothing -> return ([], t)
                    Just aTokens -> return ([aTokens], ConstType t)
    (cTokens, finalType) <- case maybeC of
                                Nothing -> return ([], afterConst)
                                Just (c, arrayT, _arrayV) -> return (c, arrayT)

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
    c@(KW_SEMICOLUMN posn) <- TT.kwSemicolumn
    optionD <- optionMaybe expStmt

    (d, expType, expValue) <- case optionD of
                Nothing -> return ([KW_TRUE posn], BoolType, Just $ BoolValue True) -- if condition is empty True will be used
                Just (d, dType, dValue) -> return (d, dType, dValue)

    assertBooleanCompatible expType posn

    e <- TT.kwSemicolumn
    setProgramState Skip -- Don't execute loop increment yet no mater what
    optionF <- optionMaybe (do
                                (f, _) <- loopIncrementStmt
                                return f
                            )
    let f = fromMaybe [] optionF
    setProgramState previousProgramState

    isRunning' <- isRunning
    condition <- if isRunning' then do
                        condition <- getBooleanValue expValue posn
                        unless condition (setProgramState Skip)
                        return condition
                        else return False

    g <- TT.kwColumn
    h <- TT.newLine
    i <- TT.indent
    openScope True
    (j, _) <- stmtList
    k <- TT.unindent

    let runFor = do
            endEarly <- endLoopEarly
            unless endEarly $ do
                closeScope 
                openScope True
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
    currProgramState <- getProgramState
    case currProgramState of
        Return {} -> return ()
        _ -> do
            setProgramState previousProgramState
            setParserBlock previousParserBlock
    closeScope
    closeScope

    return ([a] ++ b  ++ [c] ++ d ++ [e] ++ f ++ [g] ++ [h] ++ [i] ++ j ++ [k])
    where
        loopIncrementStmt :: StateType ([Token], InterpreterState)
        loopIncrementStmt = do
                                (do
                                    a <- derefAssignStmt 
                                    finalState <- getState
                                    return (a, finalState))
                                <|> (do
                                        (f, symbolId, typeList, _varV) <- var
                                        g <- assignStmt symbolId typeList
                                            <|> (do
                                                    (g, _, _) <- callStmt symbolId typeList
                                                    return g
                                                )
                                            <?> "loop increment should be either an assignStmt or a method call"
                                        finalState <- getState
                                        return (f ++ g, finalState))



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
    insertSymbol (bSymbol, underlyingType, Nothing) posn

    e <- TT.kwColumn
    f <- TT.newLine
    g <- TT.indent
    (h, _) <- stmtList
    i <- TT.unindent

    closeScope
    setParserBlock previousParserBlock
    return $ [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] ++ h ++ [i]

parser :: [Token] -> String -> IO (Maybe ParseError)
parser tokenList fileName = do
    interpreterState <- initInterpreterState fileName
    parserResult <- runParserT vectraLanguage interpreterState "Error message" tokenList
    case parserResult of
        Left a -> return $ Just a
        Right finalState -> do
            let finalProgramState = programState finalState
            case finalProgramState of
                Finished -> return Nothing
                _ -> do
                    let err = newErrorMessage (Message "Error: no main method") (initialPos "")
                    return (Just err)
