{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Parser
  ( parser
  ) where

import SymbolTable
import TerminalTokens as TT
import Scanner
import Data.Maybe
import Text.Parsec

vectraLanguage :: StateType [Token]
vectraLanguage = do
    a <- concat <$> importCommand `sepEndBy` TT.newLine
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
            blockDecl
            <|> enumDecl
            <|> funcDecl
            <|> varDecl

blockDecl :: StateType [Token]
blockDecl = do
    _ <- TT.kwBlock
    a <- TT.id
    _ <- TT.kwColumn
    _ <- TT.indent
    b <- blockList
    _ <- TT.unindent
    -- TODO insert symbol to symbolTable
    return $ a :b
    where blockList = do
            concat <$> (blockStmt `sepEndBy1` TT.kwComma)
            where
                blockStmt = do
                    _isPublic <- do
                                _ <- TT.kwPrivate
                                return False
                            <|> do
                                _ <- TT.kwPublic
                                return True
                            <|> return True
                    -- TODO check of KW_TIL, marking that the method is the function destructor, if so 
                    -- confirm that funcDecl name is the same as the block name
                    varDecl
                    <|> do
                        a <- TT.kwFunc
                        -- TODO maybe add const functions to blocks
                        b <- optionMaybe destructorDecl
                        c <- TT.id
                        d <- optionMaybe operatorSymbol
                        -- TODO if operatorSymbol is Just, id lexeme must be "operator"
                        e <- funcDeclAux
                        return $ [a] ++ fromMaybe [] b ++ [c] ++ fromMaybe [] d ++ e
                    -- TODO insert symbol to symbolTable
                destructorDecl = do
                    (:[]) <$> TT.kwTil
                operatorSymbol = do
                    (:[]) <$> (TT.opAdd 
                            <|> TT.opSub
                            <|> TT.opMult
                            <|> TT.opDiv
                            <|> TT.opAnd
                            <|> TT.opOr
                            <|> TT.opNot
                            <|> do
                                a <- TT.openBracket
                                _ <- TT.closeBracket
                                return a
                            )

enumDecl :: StateType [Token]
enumDecl = do
    _ <- TT.kwEnum
    a <- TT.id
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    b <- idList
    -- TODO insert symbol to symbolTable
    return $ a:b
    where idList = do
            concat <$> (ids `sepEndBy1` TT.newLine)
            where
            ids = do
                a <- TT.id
                return [a]

funcDecl :: StateType [Token]
funcDecl = do
    a <- TT.kwFunc
    b <- TT.id
    c <- funcDeclAux
    return $ [a] ++ [b] ++ c

funcDeclAux :: StateType [Token]
funcDeclAux = do
    _ <- TT.openParen
    c <- optVarDeclList
    _ <- TT.closeParen
    d <- optionMaybe returnDecl
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent
    return $ c ++ fromMaybe [] d ++ e
    where
        returnDecl = do
            _ <- TT.opSub
            _ <- TT.opGreater
            typeStmt

        optVarDeclList :: StateType [Token]
        optVarDeclList = do
            concat <$> (varDecl `sepBy` TT.kwComma)


varDecl :: StateType [Token]
varDecl = do
    _a <- optionMaybe TT.kwConst
    b <- typeStmt
    c <- TT.id
    d <- do
            e <- TT.kwAssingment
            f <- expStmt
            return $ e:f
        <|> return []

    return $ b ++ [c] ++ d

var :: StateType [Token]
var = do
    a <- TT.id
    b <- optionMaybe memberAccess
    -- TODO check if var exists
    return $ a:fromMaybe [] b
    where 
        memberAccess = do
            _ <- TT.kwDot
            var 

callStmt :: StateType [Token]
callStmt = do
    a <- var
    _ <- TT.openParen
    b <- expStmtList
    _ <- TT.closeParen
    -- TODO assure that exists a function called id and that there is no type error
    return $ a ++ b

literal :: StateType [Token]
literal = do
    t <- TT.intLiteral
      <|> TT.floatLiteral
      <|> TT.stringLiteral
      <|> TT.kwTrue
      <|> TT.kwFalse
    return [t]

expStmtList :: StateType [Token]
expStmtList = do
    concat <$> (expStmt `sepBy` TT.kwComma)

expStmt :: StateType [Token]
expStmt = do
    literal
    <|> typeStmt
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
    concat <$> (stmt `sepEndBy1` TT.newLine)
    where 
        stmt :: StateType [Token]
        stmt = do
            assignStmt
            <|> ifStmt
            <|> whileStmt
            <|> forStmt
            <|> foreachStmt

assignStmt :: StateType [Token]
assignStmt = do
          a <- TT.id
          b <- TT.kwAssingment
          c <- expStmt
          -- updateSymbol ("id lexema", IntType 1) -- TODO actually update the symbol table correctly
          return (a:b:c)

ifStmt :: StateType [Token]
ifStmt = do
    a <- TT.kwIf
    b <- expStmt
    c <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    d <- stmtList
    _ <- TT.unindent
    e <- optionMaybe elseStmt
    return $ [a] ++ b ++ [c] ++ d ++ fromMaybe [] e

elseStmt :: StateType [Token]
elseStmt = do
    _ <- TT.newLine
    a <- TT.kwElse
    b <- ifStmt
      <|> do
            c <- TT.kwColumn
            _ <- TT.newLine
            _ <- TT.indent
            d <- stmtList
            _ <- TT.unindent
            return $ c:d

    return $ a : b

whileStmt :: StateType [Token]
whileStmt = do
    a <- TT.kwWhile
    b <- expStmt
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    c <- stmtList -- TODO this also has to include 'continue' and 'break' so... maybe a stmtListLoop to include those kws? 
    _ <- TT.unindent
    return (a:b ++ c)


typeStmt :: StateType [Token]
typeStmt = do
        (:[]) <$> TT.kwInt
    <|> (:[]) <$> TT.kwFloat
    <|> (:[]) <$> TT.kwBool
    <|> (:[]) <$> TT.kwString
    <|> do -- refType
        a <- TT.kwRef
        _ <- TT.openParen
        b <- typeStmt
        _ <- TT.closeParen
        return (a : b)
    <|> do -- customType
        a <- TT.id
        -- TODO check if id is a valid type, i.e. if there is a block declaration that matches id in the symbol table
        return [a]

forStmt :: StateType [Token]
forStmt = do
    a <- TT.kwFor
    b <- optionMaybe varDecl
    _ <- TT.kwSemicolumn
    c <- optionMaybe expStmt
    _ <- TT.kwSemicolumn
    d <- optionMaybe expStmt
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList -- TODO this also has to include 'continue' and 'break' so... maybe a stmtListLoop to include those kws? 
    _ <- TT.unindent

    return (a : fromMaybe [] b ++ fromMaybe [] c ++ fromMaybe [] d ++ e)

foreachStmt :: StateType [Token]
foreachStmt = do
    a <- TT.kwForeach
    b <- TT.id
    c <- TT.kwIn
    d <- TT.id
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent

    return $ [a] ++ [b] ++ [c] ++ [d] ++ e

parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    -- TODO improve error message
    runParserT stmtList table_stack "Error message" token_list
