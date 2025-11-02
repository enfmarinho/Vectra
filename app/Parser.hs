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
            structDecl 
            <|> enumDecl
            <|> funcDecl
            <|> procDecl
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

procDecl :: StateType [Token]
procDecl = do
    openScope False
    a <- TT.kwProc
    (bTokens, bIds) <- option ([], []) templateDecl
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
    insertSymbol (symbolId, ProcType bIds dParams e) True

    return $ [a] ++ bTokens ++ [c] ++ dTokens ++ e

funcDecl :: StateType [Token]
funcDecl = do
    openScope False
    a <- TT.kwFunc
    (templateTokens, templateIds) <- option ([], []) templateDecl
    c <- TT.id
    (d, paramList, returnType) <- funcDeclAux

    closeScope
    let ID _posn symbolId = c
    -- TODO check for existing method with the same signature, should be a warning or an error
    insertSymbol (symbolId, FuncType templateIds paramList returnType d) True

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
    insertSymbol (symbolId, bType) False
    e <- do
            e <- TT.kwAssingment
            (f, fType, fValue) <- expStmt
            assertTypesEq bType fType posn
            -- TODO insert it's value in memory
            return $ e:f
        <|> return []

    return (b ++ [c] ++ d ++ e)

var :: StateType ([Token], Type)
var = do
    a <- TT.id
    b <- option [] memberAccess

    -- Checks if id symbol exists
    let ID posn symbolId = a
    idType <- consultType symbolId posn

    return (a:b, idType)
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
    (d, dType, dValue) <- expStmt

    let ID posn symbolId = a
    symbolType <- consultType symbolId posn

    assertTypesEq symbolType dType posn

    -- TODO handle case b is Just
    -- updateValue (symbolId, IntType 1) -- TODO actually update the symbol table correctly
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
    insertSymbol (bSymbol, underlyingType) False

    e <- TT.kwColumn
    f <- TT.newLine
    g <- TT.indent
    h <- loopStmtList
    i <- TT.unindent

    closeScope
    return $ [a] ++ [b] ++ [c] ++ [d] ++ [e] ++ [f] ++ [g] ++ h ++ [i]

parser :: [Token] -> IO (Either ParseError [Token])
parser tokenList = do
    parserState <- initParserState
    -- TODO improve error message
    runParserT vectraLanguage parserState "Error message" tokenList
