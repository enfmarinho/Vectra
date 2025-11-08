{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Parser
  ( parser
  ) where

import InterpreterState 
import TerminalTokens as TT
import Scanner
import Text.Parsec
import Types
import Assert
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import VectraLib

-- TODO some functions shouldn't return [Token], but yet do. Fix this...

vectraLanguage :: StateType [Token]
vectraLanguage = do
    a <- concat <$> (importCommand `sepEndBy` TT.newLine)
    b <- concat <$> (globalDecl `sepEndBy` TT.newLine)
    return $ a ++ b
    where
        importCommand :: StateType [Token]
        importCommand = do
            a <- TT.kwImport
            b <- do 
                    b <- TT.id
                    let ID posn symbolId = b
                    importSpecialMethod symbolId posn
                    return b
                <|> do
                    b <- TT.stringLiteral 
                    let STRING_LITERAL posn filePath = b
                    importFile filePath posn
                    return b

            return $ a:[b]

        globalDecl :: StateType [Token]
        globalDecl = do
            structDecl
            <|> implDecl
            <|> enumDecl
            <|> methodDecl
            <|> varDecl

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
        insertSymbol (symbolId, TemplateType) False
        return (a, symbolId)

templateInstanciation :: StateType ([Token], [Type])
templateInstanciation = do
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
    insertSymbol (s, t) False
    insertTemplateInstantiation typeRest symbolRest

structDecl :: StateType [Token]
structDecl = do
    openScope False
    _ <- TT.kwStruct
    (a, templateIds) <- option ([], []) templateDecl
    b <- TT.id
    _ <- TT.kwColumn
    _ <- TT.indent
    c <- concat <$> many1 varDecl -- TODO handle private and public kws
    _ <- TT.unindent
    structScope <- topScope
    closeScope
    let ID _posn symbolId = b
    insertSymbol (symbolId, StructType templateIds structScope) False
    return $ a ++ [b] ++ c

implDecl :: StateType [Token]
implDecl = do
    openScope False
    _ <- TT.kwImpl
    a <- TT.id

    let ID posn symbolId = a
    consultTypeList symbolId posn >>= assertStructType symbolId posn

    _ <- TT.kwColumn
    _ <- TT.indent
    _ <- concat <$> many1 methodDecl -- TODO handle private and public kws
    _ <- TT.unindent
    implScope <- topScope
    closeScope
    addImplMethods (symbolId, ImplNamespaceType implScope)
    return []

enumDecl :: StateType [Token]
enumDecl = do
    _ <- TT.kwEnum
    a <- TT.id
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    b <- idList

    let ID _posn enumId = a
    insertSymbol (enumId, EnumType b) False
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
            insertSymbol (symbolId, varType) False
            return (a ++ [b], (symbolId, varType))

methodDecl :: StateType [Token]
methodDecl = do
    previousParserBlock <- getParserBlock
    openScope False
    a <- TT.kwProc
        <|> TT.kwFunc
    (bTokens, bIds) <- option ([], []) templateDecl
    c <- TT.id

    let ID posn symbolId = c
    z <- getProgramState
    when (symbolId == "main") $
        if z /= Starting
            then semanticError $ "A second main method is declared here, it must exist only one " ++ showPos posn
            else setProgramState Running

    _ <- TT.openParen
    (dTokens, dParams) <- optParamDeclList
    e <- TT.closeParen
    optionF <- optionMaybe returnDecl

    case optionF of
        Nothing -> setParserBlock(Method Nothing)
        Just (_, t) -> setParserBlock(Method $ Just t)

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

            insertSymbol (symbolId, ProcType bIds dParams g) True
        KW_FUNC _ -> case optionF of
                        Nothing -> semanticError $ "A function must return something. Consider declaring "
                                                    ++ symbolId ++ " as a procedure instead " ++ showPos posn
                        Just (_, returnType) -> insertSymbol (symbolId, FuncType bIds dParams returnType g) True
        _ ->  fail "<methodDecl>" -- Impossible to get here, this is just to avoid warnings  

    when (symbolId == "main") $
        setProgramState Finished
    setParserBlock previousParserBlock
    return ([a] ++ bTokens ++ [c] ++ dTokens ++ [e] ++ maybe [] fst optionF ++ g)

arrayDecl :: StateType ([Token], Int)
arrayDecl = do
    a <- TT.openBracket
    (b, _bType, bValue) <- expStmt
    let OPEN_BRACKET posn = a
    arraySize <- assertNumberTypeReturnInt bValue posn
    c <- TT.closeBracket
    return ([a] ++ b ++ [c], arraySize)

returnDecl :: StateType ([Token], Type)
returnDecl = do
    _ <- TT.opSub
    _ <- TT.opGreater
    (a, aType) <- typeStmt
    optionB <- optionMaybe arrayDecl
    (b, returnType) <- case optionB of
        Nothing -> return ([], aType)
        Just (b, arraySize) -> return (b, ArrayType arraySize aType)
    return (a ++ b, returnType)

varDecl :: StateType [Token]
varDecl = do
    a <- optionMaybe TT.kwLocal
    (b, bType) <- typeStmt
    c <- TT.id
    optionD <- optionMaybe arrayDecl
    (d, varType) <- case optionD of
                        Nothing -> return ([], bType)
                        Just (d, arraySize) -> return (d, ArrayType arraySize bType)
    let ID posn symbolId = c
    when (isNothing a) $ checkShadowing symbolId posn
    insertSymbol (symbolId, varType) False
    e <- do
            e <- TT.kwAssingment
            (f, fType, fValue) <- expStmt
            assertTypesEq varType fType posn
            insertValue (symbolId, fValue)
            return $ e:f
        <|> return []

    return (b ++ [c] ++ d ++ e)

var :: StateType ([Token], Type, Value)
var = do
    -- TODO this in bugged
    -- TODO allow [] 
    a <- TT.id
    next <- optionMaybe $ do
        _ <- TT.kwDot
        var
    let ID posn symbolId = a
    case next of
        Nothing -> do
            t <- consultType symbolId posn
            maybeValue <- consultValue symbolId
            value <- case maybeValue of
                        Nothing -> semanticError $ "Trying to use uninitialized symbol " ++ symbolId ++ " " ++ showPos posn
                        Just v -> return v

            return ([a], t, value)
        Just (bTokens, bType, bValue) ->
            return (a:bTokens, bType, bValue)

callStmt :: StateType ([Token], Maybe Type, Maybe Value)
callStmt = do
    previousProgramState <- getProgramState
    openScope False
    (a, symbolType, _) <- var -- TODO maybe create a new rule for this, since it's a bit different than var itself
    (b, templateTypeList) <- option ([], []) templateInstanciation
    c <- TT.openParen

    -- TODO this is wrong, must search [Type] for valid method to allow method override
    let OPEN_PAREN posn = c
    (templateIds, paramList, maybeReturnType, funcBody) <- case symbolType of
                        FuncType _templateIds paramList returnType _funcBody -> return (_templateIds, paramList, Just returnType, _funcBody)
                        ProcType _templateIds paramList _funcBody -> return (_templateIds, paramList, Nothing, _funcBody)
                        _ -> semanticError $ "Trying to call type " ++ show symbolType ++ ", it must be a function or procedure "  ++ showPos posn

    -- Check and instantiate templates and parameters
    (d, typeList, valueList) <- unzip3 <$> expStmtList
    let (idList, expectedParamTypes) = unzip paramList
    assertValidParamList expectedParamTypes typeList posn
    instatiateArgs idList typeList valueList
    insertTemplateInstantiation templateTypeList templateIds

    e <- TT.closeParen

    returnValue <- runFuncBody funcBody

    setProgramState previousProgramState
    closeScope
    return (a ++  b ++ [c] ++ concat d ++ [e], maybeReturnType, returnValue)
    where 
        instatiateArgs :: [String] -> [Type] -> [Value] -> StateType ()
        instatiateArgs (idListHead:isListTail) (typeListHead:typeListTail) (valueListHead:valueListTail) = do
            insertSymbol (idListHead, typeListHead) False
            insertValue (idListHead, valueListHead)
            instatiateArgs isListTail typeListTail valueListTail
        instatiateArgs [] [] [] = return ()
        instatiateArgs [] _ _ = fail "<callStmt>"
        instatiateArgs _ [] _ = fail "<callStmt>"
        instatiateArgs _ _ [] = fail "<callStmt>"

        
        runFuncBody :: [Token] -> StateType (Maybe Value)
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
                    maybeReturnV <- case currProgramState of
                                        Return returnValue -> do
                                            -- TODO check for mismatch between expected type and returned type
                                            return returnValue
                                        _ -> do
                                            -- TODO confirm its a call for a procedure, otherwise emit warning for function with no return on control path
                                            return Nothing 
                    setProgramState previousProgramState
                    return maybeReturnV
            

literal :: StateType ([Token], Type, Value)
literal = do
    do
        a <- TT.intLiteral
        let INT_LITERAL _ v = a
        return ([a], IntType, IntValue v)
    <|> do
        a <- TT.floatLiteral
        let FLOAT_LITERAL _ v = a
        return ([a], FloatType, FloatValue v)
    <|> do
        a <- TT.stringLiteral
        let STRING_LITERAL _ v = a
        return ([a], StringType, StringValue v)
    <|> do
        a <- TT.kwTrue
        return ([a], BoolType, BoolValue True)
    <|> do
        a <- TT.kwFalse
        return ([a], BoolType, BoolValue False)
        -- TODO implement literal for list 

expStmtList :: StateType [([Token], Type, Value)]
expStmtList = do
    -- TODO
    -- concat <$> (expStmt `sepBy` TT.kwComma)
    return []

baseExp :: StateType ([Token], Type, Value)
baseExp = do
    optionUnary <- optionMaybe (TT.opSub <|> TT.opNot)
    (base, baseT, baseV) <- literal 
                            <|> try var
                            <|> do
                                (a, maybeType, maybeValue) <- callStmt
                                expValue <- case maybeValue of
                                                Nothing -> semanticError "TODO calling procedure and expecting a value"
                                                Just v -> return v
                                expType <- case maybeType of 
                                                Nothing -> semanticError "TODO calling procedure and expecting a return type" -- will never reach this
                                                Just t -> return t
                                return (a, expType, expValue)
                            <|> do 
                                a <- TT.openParen
                                (b, expType, expValue) <- expStmt
                                c <- TT.closeParen
                                return ([a] ++ b ++ [c], expType, expValue)
                            -- <|> do
                            --     a <- TT.kwCast
                            --     b <- TT.opSmaller
                            --     (c, t) <- typeStmt
                            --     d <- TT.opGreater
                            --     e <- TT.openParen
                            --     f <- var
                            --     g <- TT.closeParen
                                -- return ([a] ++ [b] ++ c ++ [d], t, Maybe)
                            -- <|> do -- TODO derefVar
                            --     a <- TT.kwDeref
                            --     b <- TT.openParen
                            --     (c, varType, _varValue) <- var
                            --     let OPEN_PAREN posn = b
                            --     derefType <- case varType of
                            --                     RefType derefType -> return derefType
                            --                     _ -> semanticError $ "Trying to deref a non reference type " ++ showPos posn
                            --     d <- TT.closeParen
                            --
                                -- derefValue <- case varValue of
                                --                 RefValue key refId -> return 
                                --                 _ -> semanticError $ "Trying to deref a non reference type " ++ showPos posn
                                -- return ([a] ++ [b] ++ c ++ [d], derefType, IntValue 1)

    case optionUnary of
        Nothing -> return (base, baseT, baseV)
        Just unary ->
            case unary of
                OP_SUB posn -> do
                    (_, resultV) <- handleUnaryMinus baseV posn
                    return (unary : base, baseT, resultV)
                OP_NOT posn -> do
                    (_, resultV) <- handleNot baseV posn
                    return (unary : base, baseT, resultV)
                _ -> return (base, baseT, baseV)


orExpStmt :: StateType ([Token], Type, Value)
orExpStmt = do
    (a, at, av) <- andExpStmt
    option (a, at, av) (do
            b <- TT.opOr 
            let OP_OR posn = b
            (c, _, v) <- orExpStmt

            (resultT, resultV) <- handleOr av v posn
            return (a ++ [b] ++ c, resultT, resultV)
        )
            

andExpStmt :: StateType ([Token], Type, Value)
andExpStmt = do
    (a, at, av) <- compareExpStmt 
    option (a, at, av) (do
            b <- TT.opAnd 
            let OP_AND posn = b
            (c, _, v) <- andExpStmt

            (resultT, resultV) <- handleAnd av v posn
            return (a ++ [b] ++ c, resultT, resultV)
        )

compareExpStmt :: StateType ([Token], Type, Value)
compareExpStmt = do
    (a, at, av) <- addSubExpStmt 
    option (a, at, av) (do
            -- TODO Yeap, I was wrong... It's better to have only one token for every comparison and make it store the lexeme
            (b, _posn) <- do
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
            (c, _, _v) <- addSubExpStmt

            -- (resultT, resultV) <- handleComparison av v posn
            -- return (a ++ [b] ++ c, resultT, resultV)
            return (a ++ [b] ++ c, at, av)
        )
    

addSubExpStmt :: StateType ([Token], Type, Value)
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
            (c, _, v) <- addSubExpStmt

            (resultT, resultV) <- if isAdd
                                    then handleAdd av v posn
                                    else handleSub av v posn
            return (a ++ [b] ++ c, resultT, resultV)
        )

multDivExpStmt :: StateType ([Token], Type, Value)
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
            (c, _, v) <- multDivExpStmt

            (resultT, resultV) <- if isMult
                                    then handleMult av v posn
                                    else handleDiv av v posn
            return (a ++ [b] ++ c, resultT, resultV)
        )


expStmt :: StateType ([Token], Type, Value)
expStmt = do
    orExpStmt

stmtList :: StateType ([Token], InterpreterState)
stmtList = do
    _ <- optionMaybe TT.newLine
    a <- stmt
    b <- concat <$> many (try $ do 
            b <- TT.newLine
            c <- stmt
            return (b:c)
        )
    _ <- optionMaybe TT.newLine
    st <- getState
    return (a ++ b, st)

stmt :: StateType [Token]
stmt = do
    try $ do
        (a, _) <- assignStmt
        return a
    <|> do
        (a, _) <- ifStmt
        return a
    <|> whileStmt
    <|> forStmt
    <|> foreachStmt
    <|> try (do
        (a, _, _) <- callStmt
        return a)
    <|> varDecl
    <|> do
        a <- TT.kwContinue
        let KW_CONTINUE posn = a
        assertContinuable posn
        setProgramState Skip
        return [a]
    <|> do
        a <- TT.kwBreak
        let KW_BREAK posn = a
        assertBreakable posn
        setProgramState Break
        return [a]
    <|> do
        a <- TT.kwReturn 
        optionB <- optionMaybe expStmt
        let KW_RETURN posn = a
        assertReturnable posn
        b <- case optionB of
                Nothing -> return []
                Just (b, expType, expValue) -> do
                    assertReturnType expType posn
                    isRunning' <- isRunning
                    when isRunning' $ do setProgramState $ Return (Just expValue)
                    return b

        return $ a:b
        

mathOpSymbol :: StateType [Token]
mathOpSymbol = do
    t <- TT.opAdd
        <|> TT.opSub
        <|> TT.opMult
        <|> TT.opDiv
        <|> TT.opAnd
        <|> TT.opOr
        <|> TT.opNot
    return [t]

assignStmt :: StateType ([Token], InterpreterState)
assignStmt = do
    a <- TT.id
    optionB <- optionMaybe mathOpSymbol
    c <- TT.kwAssingment
    (d, expType, expValue) <- expStmt

    let ID posn symbolId = a
    symbolType <- consultType symbolId posn

    assertTypesEq symbolType expType posn

    isRunning' <- isRunning
    when isRunning' $ do
        value <- case optionB of
                        Nothing -> return expValue
                        Just [op] -> do
                                    maybeValue <- consultValue symbolId
                                    value <- case maybeValue of
                                        Nothing -> semanticError $ "Trying to use " ++ symbolId ++ " without initializing it " ++ showPos posn
                                        Just value -> return value
                                    do
                                        resultValue <- case op of
                                            OP_ADD _ -> handleAdd value expValue posn
                                            OP_SUB _ -> handleSub value expValue posn
                                            OP_MULT _ -> handleMult value expValue posn
                                            OP_DIV _ -> handleDiv value expValue posn
                                            OP_AND _ -> handleAnd value expValue posn
                                            OP_OR _ -> handleOr value expValue posn
                                            _ -> semanticError $ "Invalid operation on assignment operation for " ++ symbolId ++ " " ++ showPos posn
                                        castValueToType symbolType resultValue posn
                        Just _ -> fail "<assignStmt>"
        updateValue (symbolId, value)

    currInterpreterState <- getState
    return ([a] ++ fromMaybe [] optionB ++ [c] ++ d, currInterpreterState)

-- The Bool indicates whether the conditional was executed
ifStmt :: StateType ([Token], Bool)
ifStmt = do
    previousProgramState <- getProgramState
    previousParserBlock <- getParserBlock
    openScope True
    a <- TT.kwIf
    setParserBlock Conditional
    (b, expType, expValue) <- expStmt

    let KW_IF posn = a
    assertBooleanCompatible expType posn
    expBool <- getBooleanValue expValue

    let conditional = expBool && previousProgramState == Running
    unless conditional $ do setProgramState Skip

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    (f, _) <- stmtList
    g <- TT.unindent
    _ <- TT.newLine

    if conditional then
        setProgramState Skip
        else setProgramState previousProgramState

    h <- concat <$> many (try $ do
            (h, executed) <- elseIfStmt
            when executed $ setProgramState Skip
            return h
        )

    i <- option [] elseStmt

    closeScope
    setProgramState previousProgramState
    setParserBlock previousParserBlock
    return ([a] ++ b ++ [c] ++ [d] ++ [e] ++ f ++ [g] ++ h ++ i, conditional)
    where
        -- The Bool indicates whether the conditional was executed
        elseIfStmt :: StateType ([Token], Bool)
        elseIfStmt = do
            b <- TT.kwElse
            (c, executed) <- ifStmt
            return (b:c, executed)
        elseStmt :: StateType [Token]
        elseStmt = do
            openScope True
            b <- TT.kwElse
            c <- TT.kwColumn
            d <- TT.newLine
            e <- TT.indent
            (f, _) <- stmtList
            g <- TT.unindent
            closeScope
            return $ [b] ++ [c] ++ [d] ++ [e] ++ f ++ [g]


evaluateBooleanExp :: [Token] -> StateType (Value, InterpreterState)
evaluateBooleanExp tokenList = do
    previusState <- getState

    parserExpResult <- liftIO $ runParserT expStmt previusState "<evaluateBooleanExp>" tokenList
    expValue <- case parserExpResult of
                        Left _ -> fail "<evaluateBooleanExp>"
                        Right (_, _, expValue) -> return expValue
    resultState <- getState
    return (expValue, resultState)

whileStmt :: StateType [Token]
whileStmt = do
    previousProgramState <- getProgramState
    previousParserBlock <- getParserBlock
    openScope True

    a <- TT.kwWhile
    setParserBlock Loop
    (b, expType, expValue) <- expStmt

    let KW_WHILE posn = a

    assertBooleanCompatible expType posn
    condition <- getBooleanValue expValue
    unless condition $ setProgramState Skip

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    (f, _) <- stmtList
    g <- TT.unindent

    let _runWhile = do
            currProgramState <- getProgramState
            when (currProgramState == Break || currProgramState == Return {} || previousProgramState /= Running) $ return () -- TODO bug
            setProgramState previousProgramState

            (expValue', resultState) <- evaluateBooleanExp b
            condition' <- getBooleanValue expValue'
            setState resultState
            
            when condition' $ do
                st <- getState
                parserResult <- liftIO $ runParserT stmtList st "<while>" f
                case parserResult of
                    Left _ -> fail "<while>"
                    Right (_, resultState') -> putState resultState'
                _runWhile

    -- when condition runWhile
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
                t <- consultTypeList s posn >>= getEnumOrStructTypes s posn
                return ([b], t)
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

    (aTokens, finalType) <- case a of
                    Nothing -> return ([], t)
                    Just aTokens -> return ([aTokens], ConstType t)

    return (aTokens ++ b, finalType)
    where constDecl = TT.kwConst

forStmt :: StateType [Token]
forStmt = do
    previousParserBlock <- getParserBlock
    previousProgramState <- getProgramState
    openScope True
    a <- TT.kwFor
    setParserBlock Loop
    b <- option [] varDecl
    c <- TT.kwSemicolumn
    optionD <- optionMaybe expStmt

    let KW_SEMICOLUMN posn = c

    (d, expType, expValue) <- case optionD of
                Nothing -> return ([KW_TRUE posn], BoolType, BoolValue True) -- if conditional is empty True will be used
                Just (d, dType, dValue) -> return (d, dType, dValue)

    assertBooleanCompatible expType posn

    e <- TT.kwSemicolumn
    setProgramState Skip -- Don't execute assignStmt yet no mater what
    optionF <- optionMaybe assignStmt -- TODO this can also be a callStmt
    setProgramState previousProgramState

    f <- case optionF of
            Nothing -> return []
            Just (f, _) -> return f

    condition <- getBooleanValue expValue
    unless condition $ setProgramState Skip

    g <- TT.kwColumn
    h <- TT.newLine
    i <- TT.indent
    (j, _) <- stmtList
    k <- TT.unindent

    let _runFor = do
            currProgramState <- getProgramState
            when (currProgramState == Break || currProgramState == Return {} || previousProgramState /= Running) $ return () -- TODO bug
            setProgramState previousProgramState

            -- Perform assignStmt, i.e. operation to be performed after loop
            st <- getState
            parserResultAssingStmt <- liftIO $ runParserT assignStmt st "<for>" f
            case parserResultAssingStmt of
                Left _ -> fail "<for>"
                Right (_, resultState') -> putState resultState'

            (expValue', resultState) <- evaluateBooleanExp d
            condition' <- getBooleanValue expValue'
            setState resultState
            
            when condition' $ do
                st' <- getState
                parserResult <- liftIO $ runParserT stmtList st' "<for>" f
                case parserResult of
                    Left _ -> fail "<for>"
                    Right (_, resultState') -> putState resultState'

                _runFor

    -- when condition runFor
    setProgramState previousProgramState
    setParserBlock previousParserBlock
    closeScope

    return ([a] ++ b  ++ [c] ++ d ++ [e] ++ f ++ [g] ++ [h] ++ [i] ++ j ++ [k])

foreachStmt :: StateType [Token]
foreachStmt = do
    previousParserBlock <- getParserBlock
    openScope True
    a <- TT.kwForeach
    setParserBlock Loop
    b <- TT.id
    c <- TT.kwIn
    d <- TT.id

    let ID posn dSymbol = d
    dType <- consultType dSymbol posn
    assertIterableType dSymbol dType posn

    let ArrayType _size underlyingType = dType
    let ID _ bSymbol = b
    insertSymbol (bSymbol, underlyingType) False

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
    -- TODO improve error message
    runParserT vectraLanguage interpreterState "Error message" tokenList
