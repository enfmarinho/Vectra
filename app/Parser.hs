module Parser
  ( parser
  ) where

import ParserState
import TerminalTokens as TT
import Scanner
import Text.Parsec
import Types
import Assert

vectraLanguage :: StateType [Token]
vectraLanguage = do
    a <- concat <$> (importCommand `sepEndBy` TT.newLine)
    b <- concat <$> (globalDecl `sepEndBy` TT.newLine)
    return $ a ++ b
    where
        importCommand :: StateType [Token]
        importCommand = do
            a <- TT.kwImport
            b <- TT.id
                <|> TT.stringLiteral
            return $ a:[b]

        globalDecl :: StateType [Token]
        globalDecl = do
            -- structDecl<|> 
            enumDecl
            <|> funcDecl
            <|> procDecl
            <|> varDecl

template :: StateType ([Token], [String])
template = do
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
        insertSymbol (symbolId, TemplateType)
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
    insertSymbol (s, t)
    insertTemplateInstantiation typeRest symbolRest


-- structDecl :: StateType [Token]
-- structDecl = do
--     _ <- TT.kwStruct
--     optionA <- optionMaybe template
--     b <- TT.id
--     _ <- TT.kwColumn
--     _ <- TT.indent
--     -- TODO insert templates in symbolTable in case there are
--     c <- structList
--     _ <- TT.unindent
--     -- TODO insert symbol to symbolTable
--     return $ fromMaybe [] a ++ [b] ++ c
--     where
--         structList = do
--             concat <$> (structStmt `sepEndBy1` TT.newLine)
--             where
--                 structStmt = do
--                     _isPublic <- do
--                                 _ <- TT.kwPrivate
--                                 return False
--                             <|> do
--                                 _ <- TT.kwPublic
--                                 return True
--                             <|> return True
--
--                     varDecl
--                     <|> procDecl
--                     <|> do
--                         a <- TT.kwFunc
--                         -- TODO maybe add const functions to structs
--                         -- TODO can i use option here ?
--                         b <- option [] destructorDecl
--                             <|> option [] template
--
--                         c <- TT.id
--                         -- TODO check if b is not Nothing, if so assure that c lexeme is the same as the struct name
--                         d <- optionMaybe operatorSymbol
--                         -- TODO if operatorSymbol is Just, id lexeme must be "operator"
--                         (e, t) <- funcDeclAux
--                         return $ [a] ++ b ++ [c] ++ fromMaybe [] d ++ e
--                     -- TODO insert symbol to symbolTable
--                 destructorDecl = do
--                     (:[]) <$> TT.kwTil
--                 operatorSymbol = do
--                     a <- optionMaybe mathOpSymbol
--                     case a of
--                         Just s -> return s
--                         Nothing -> do
--                             (:[]) <$> (TT.opSmaller
--                                     <|> TT.opSmallerEq
--                                     <|> TT.opGreater
--                                     <|> TT.opGreaterEq
--                                     <|> TT.opEq
--                                     <|> TT.opNotEq
--                                     <|> do
--                                         -- Only one of those token is enough to identify the operations, returning 
--                                         -- both of them would be unnecessary and annoying to do
--                                         _ <- TT.openBracket
--                                         TT.closeBracket
--                                     )

enumDecl :: StateType [Token]
enumDecl = do
    _ <- TT.kwEnum
    a <- TT.id
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    b <- idList

    let ID _posn enumId = a
    insertSymbol (enumId, EnumType b)
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
            insertSymbol (symbolId, varType)
            return (a ++ [b], (symbolId, varType))

procDecl :: StateType [Token]
procDecl = do
    openScope False
    a <- TT.kwProc
    (bTokens, bIds) <- option ([], []) template
    c <- TT.id
    _ <- TT.openParen
    (dTokens, dParams) <- optParamDeclList
    _ <- TT.closeParen
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent

    closeScope

    let ID _posn symbolId = c
    insertSymbol (symbolId, ProcType bIds dParams e)

    return $ [a] ++ bTokens ++ [c] ++ dTokens ++ e

funcDecl :: StateType [Token]
funcDecl = do
    openScope False
    a <- TT.kwFunc
    (templateTokens, templateIds) <- option ([], []) template
    c <- TT.id
    (d, paramList, returnType) <- funcDeclAux

    closeScope
    let ID _posn symbolId = c
    insertSymbol (symbolId, FuncType templateIds paramList returnType d)

    return $ [a] ++ templateTokens ++ [c] ++ d

funcDeclAux :: StateType ([Token], [(String, Type)], Type)
funcDeclAux = do
    _ <- TT.openParen
    (c, paramList) <- optParamDeclList
    _ <- TT.closeParen
    (d, returnType) <- returnDecl
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent

    return (c ++ d ++ e, paramList, returnType)

arrayDecl :: StateType ([Token], Int)
arrayDecl = do
    a <- TT.openBracket
    (b, bType, bValue) <- expStmt
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
    (b, bType) <- typeStmt
    c <- TT.id
    optionD <- optionMaybe arrayDecl
    (d, varType) <- case optionD of
                        Nothing -> return ([], bType)
                        Just (d, arraySize) -> return (d, ArrayType arraySize bType)
    let ID posn symbolId = c
    checkShadowing symbolId posn
    insertSymbol (symbolId, bType)
    e <- do
            e <- TT.kwAssingment
            (f, f_type, f_value) <- expStmt
            assertTypesEq bType f_type posn
            -- TODO insert it's value in memory
            return $ e:f
        <|> return []

    return (b ++ [c] ++ d ++ e)

var :: StateType ([Token], Type)
var = do
    a <- TT.id
    b <- option [] memberAccess

    -- Checks if id symbol exists
    let ID _ symbol_id = a
    consultResult <- consultSymbol symbol_id
    id_type <- case consultResult of
                    Nothing -> semanticError "asd"
                    Just v -> return v

    return (a:b, id_type)
    where
        memberAccess :: StateType [Token]
        memberAccess = do
            a <- TT.kwDot
            (b, _) <- var
            return $ a:b

callStmt :: StateType [Token]
callStmt = do
    openScope False
    (a, symbolType) <- var -- TODO var cannot be used in this context

    (templateIds, paramList, _funcBody) <- case symbolType of
                        FuncType _templateIds paramList _ _funcBody -> return (_templateIds, paramList, _funcBody)
                        ProcType _templateIds paramList _funcBody -> return (_templateIds, paramList, _funcBody)
                        _ -> semanticError "TODO funcCallStmt err msg"

    (b, templateTypeList) <- option ([], []) templateInstanciation
    insertTemplateInstantiation templateTypeList templateIds

    c <- TT.openParen
    (d, typeList, valueList) <- unzip3 <$> expStmtList
    e <- TT.closeParen

    let OPEN_PAREN posn = c
    let (_, expectedParamTypes) = unzip paramList
    assertValidParamList expectedParamTypes typeList posn

    -- TODO Instantiate args
    -- TODO actually run the function body

    closeScope
    return (a ++  b ++ [c] ++ concat d ++ [e])

funcCallStmt :: StateType ([Token], Type)
funcCallStmt = do
    openScope False
    (a, symbolType) <- var

    (templateIds, paramList, returnType, _funcBody) <- case symbolType of
                        FuncType _templateIds paramList returnType _funcBody -> return (_templateIds, paramList, returnType, _funcBody)
                        _ -> semanticError "TODO funcCallStmt err msg"

    (b, templateTypeList) <- option ([], []) templateInstanciation
    insertTemplateInstantiation templateTypeList templateIds

    c <- TT.openParen
    (d, typeList, valueList) <- unzip3 <$> expStmtList
    e <- TT.closeParen

    let OPEN_PAREN posn = c
    let (_, expectedParamTypes) = unzip paramList
    assertValidParamList expectedParamTypes typeList posn

    -- TODO Instantiate args
    -- TODO actually run the function body

    closeScope
    return (a ++  b ++ [c] ++ concat d ++ [e], returnType)

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

expStmtList :: StateType [([Token], Type, Value)]
expStmtList = do
    -- TODO
    -- concat <$> (expStmt `sepBy` TT.kwComma)
    return []

-- TODO allow [] 
expStmt :: StateType ([Token], Type, Value)
expStmt = do
    literal
    -- <|> do -- ComparisonExp
    --     a <- expDecl
    --     b <- TT.opCompare
    --     c <- expDecl
    --     return (a ++ [b] ++ c)
    -- <|> do -- refVarExp
    --     a <- TT.kwRef
    --     _ <- TT.openParen
    --     b <- TT.id
    --     _ <- TT.closeParen
    --     return $ a:[b] 
    -- <|> do -- derefVarExp
    --     a <- TT.kwRef
    --     _ <- TT.openParen
    --     b <- TT.id
    --     _ <- TT.closeParen
    --     return $ a:[b] 
    -- <|> do -- varExp
    --     -- TODO
    --     return []
    -- <|> do -- (exp)
    --     _ <- TT.openParen
    --     a <- expDecl
    --     _ <- TT.closeParen
    --     return a
    -- <|> callStmt

stmtList :: StateType [Token]
stmtList = do
    _ <- optionMaybe TT.newLine
    concat <$> many (do
        a <- stmt
        b <- TT.newLine
        return (a ++ [b]))

loopStmtList :: StateType [Token]
loopStmtList = do
    _ <- optionMaybe TT.newLine
    concat <$> many (do
        a <- stmt
            <|> (:[]) <$> TT.kwContinue
            <|> (:[]) <$> TT.kwBreak
        b <- TT.newLine
        return (a ++ [b]))

stmt :: StateType [Token]
stmt = do
    try assignStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> foreachStmt
    <|> try callStmt

mathOpSymbol :: StateType [Token]
mathOpSymbol = do
    (:[]) <$> (TT.opAdd
            <|> TT.opSub
            <|> TT.opMult
            <|> TT.opDiv
            <|> TT.opAnd
            <|> TT.opOr
            <|> TT.opNot)

assignStmt :: StateType [Token]
assignStmt = do
    a <- TT.id
    b <- option [] mathOpSymbol
    c <- TT.kwAssingment
    (d, d_type, d_value) <- expStmt

    let ID posn symbol_id = a
    symbolType <- consultType symbol_id posn

    assertTypesEq symbolType d_type posn

    -- TODO handle case b is Just
    -- updateValue (symbol_id, IntType 1) -- TODO actually update the symbol table correctly
    return $ [a] ++ b ++ [c] ++ d

ifStmt :: StateType [Token]
ifStmt = do
    openScope True
    a <- TT.kwIf
    (b, expType, expValue) <- expStmt

    let KW_IF posn = a
    assertBooleanCompatible expType posn

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    f <- stmtList
    g <- TT.unindent
    h <- option [] elseStmt

    closeScope
    return $ [a] ++ b ++ [c] ++ [d] ++ [e] ++ f ++ [g] ++ h

elseStmt :: StateType [Token]
elseStmt = do
    openScope True
    a <- TT.newLine
    b <- TT.kwElse
    c <- ifStmt
      <|> do
            d <- TT.kwColumn
            e <- TT.newLine
            f <- TT.indent
            g <- stmtList
            h <- TT.unindent
            return $ [d] ++ [e] ++ [f] ++ g ++ [h]

    closeScope
    return $ [a] ++ [b] ++ c

whileStmt :: StateType [Token]
whileStmt = do
    openScope True
    a <- TT.kwWhile
    (b, expType, expValue) <- expStmt

    let KW_WHILE posn = a
    assertBooleanCompatible expType posn

    c <- TT.kwColumn
    d <- TT.newLine
    e <- TT.indent
    f <- loopStmtList
    g <- TT.unindent

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
                return ([b], FloatType)
            <|> do
                b <- TT.kwString
                return ([b], FloatType)
            <|> do -- refType
                b <- TT.kwRef
                c <- TT.openParen
                (d, t) <- typeStmt
                e <- TT.closeParen
                return ([b] ++ [c] ++ d ++ [e], RefType t)
            <|> do -- customType
                b <- TT.id
                let ID posn s = b
                t <- consultType s posn
                return ([b], t)
            <|> do -- reference for method
                b <- TT.openParen
                (c, templateIds) <- option ([], []) template
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
    openScope True
    a <- TT.kwFor
    b <- option [] varDecl
    c <- TT.kwSemicolumn
    optionD <- optionMaybe expStmt

    (d, dType, dValue) <- case optionD of
                Nothing -> return ([], BoolType, BoolValue True) -- Bool and True are returned as a workaround
                Just (d, dType, dValue) -> return (d, dType, dValue)

    let KW_SEMICOLUMN posn = c
    assertBooleanCompatible dType posn

    e <- TT.kwSemicolumn
    optionF <- optionMaybe expStmt
    g <- TT.kwColumn
    h <- TT.newLine
    i <- TT.indent
    j <- loopStmtList
    k <- TT.unindent

    f <- case optionF of
            Nothing -> return []
            Just (f, _, _) -> return f
    closeScope
    return ([a] ++ b  ++ [c] ++ d ++ [e] ++ f ++ [g] ++ [h] ++ [i] ++ j ++ [k])

foreachStmt :: StateType [Token]
foreachStmt = do
    openScope True
    a <- TT.kwForeach
    b <- TT.id
    c <- TT.kwIn
    d <- TT.id

    let ID posn dSymbol = d
    dType <- consultType dSymbol posn
    assertIterableType dSymbol dType posn

    let ArrayType _size underlyingType = dType
    let ID _ bSymbol = b
    insertSymbol (bSymbol, underlyingType)

    e <- TT.kwColumn
    f <- TT.newLine
    g <- TT.indent
    h <- loopStmtList
    i <- TT.unindent

    closeScope
    return $ [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] ++ h ++ [i]

parser :: [Token] -> IO (Either ParseError [Token])
parser token_list = do
    parserState <- initParserState
    -- TODO improve error message
    runParserT vectraLanguage parserState "Error message" token_list
