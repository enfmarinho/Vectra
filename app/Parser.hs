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
            blockDecl
            <|> enumDecl
            <|> funcDecl
            <|> procDecl
            <|> varDecl

template :: StateType [Token]
template = do
    _ <- TT.opSmaller
    a <- concat <$> (idSymbol `sepEndBy1` TT.kwComma)
    _ <- TT.opGreater
    return a
    where idSymbol = (:[]) <$> TT.id

blockDecl :: StateType [Token]
blockDecl = do
    _ <- TT.kwBlock
    a <- optionMaybe template
    b <- TT.id
    _ <- TT.kwColumn
    _ <- TT.indent
    -- TODO insert templates in symbolTable in case there are
    c <- blockList
    _ <- TT.unindent
    -- TODO insert symbol to symbolTable
    return $ fromMaybe [] a ++ [b] ++ c
    where
        blockList = do
            concat <$> (blockStmt `sepEndBy1` TT.newLine)
            where
                blockStmt = do
                    _isPublic <- do
                                _ <- TT.kwPrivate
                                return False
                            <|> do
                                _ <- TT.kwPublic
                                return True
                            <|> return True
                    varDecl
                    <|> procDecl
                    <|> do
                        a <- TT.kwFunc
                        -- TODO maybe add const functions to blocks
                        -- TODO can i use option here ?
                        b <- option [] destructorDecl
                            <|> option [] template

                        c <- TT.id
                        -- TODO check if b is not Nothing, if so assure that c lexeme is the same as the block name
                        d <- optionMaybe operatorSymbol
                        -- TODO if operatorSymbol is Just, id lexeme must be "operator"
                        e <- funcDeclAux
                        return $ [a] ++ b ++ [c] ++ fromMaybe [] d ++ e
                    -- TODO insert symbol to symbolTable
                destructorDecl = do
                    (:[]) <$> TT.kwTil
                operatorSymbol = do
                    a <- optionMaybe mathOpSymbol
                    case a of
                        Just s -> return s
                        Nothing -> do
                            (:[]) <$> (TT.opSmaller
                                    <|> TT.opSmallerEq
                                    <|> TT.opGreater
                                    <|> TT.opGreaterEq
                                    <|> TT.opEq
                                    <|> TT.opNotEq
                                    <|> do
                                        -- Only one of those token is enough to identify the operations, returning 
                                        -- both of them would be unnecessary and annoying to do
                                        _ <- TT.openBracket
                                        TT.closeBracket
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

optVarDeclList :: StateType [Token]
optVarDeclList = do
    concat <$> (varDecl `sepBy` TT.kwComma)

procDecl :: StateType [Token]
procDecl = do
    a <- TT.kwProc
    b <- optionMaybe template
    c <- TT.id
    _ <- TT.openParen
    d <- optVarDeclList
    _ <- TT.closeParen
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent
    return $ [a] ++ fromMaybe [] b ++ [c] ++ d ++ e

funcDecl :: StateType [Token]
funcDecl = do
    a <- TT.kwFunc
    b <- optionMaybe template
    c <- TT.id
    d <- funcDeclAux
    return $ [a] ++ fromMaybe [] b ++ [c] ++ d

arrayDecl :: StateType [Token]
arrayDecl = do
    _ <- TT.openBracket
    a <- expStmt
    _ <- TT.closeBracket
    return a

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
            a <- typeStmt
            b <- optionMaybe arrayDecl
            return $ a ++ fromMaybe [] b

varDecl :: StateType [Token]
varDecl = do
    _a <- optionMaybe TT.kwConst
    b <- typeStmt
    c <- TT.id
    d <- optionMaybe arrayDecl
    e <- do
            e <- TT.kwAssingment
            f <- expStmt
            return $ e:f
        <|> return []

    return $ b ++ [c] ++ fromMaybe [] d ++ e

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

-- TODO allow [] 
expStmt :: StateType [Token]
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
    concat <$> (stmt `sepEndBy1` TT.newLine)

loopStmtList :: StateType [Token]
loopStmtList = do
    _ <- optionMaybe TT.newLine
    concat <$> (loopStmt `sepEndBy1` TT.newLine)
    where
        loopStmt :: StateType [Token]
        loopStmt = do
            stmt
            <|> (:[]) <$> TT.kwContinue
            <|> (:[]) <$> TT.kwBreak

stmt :: StateType [Token]
stmt = do
    assignStmt
    <|> ifStmt
    <|> whileStmt
    <|> forStmt
    <|> foreachStmt
    <|> callStmt

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
          b <- optionMaybe mathOpSymbol
          c <- TT.kwAssingment
          d <- expStmt
          -- updateSymbol ("id lexema", IntType 1) -- TODO actually update the symbol table correctly
          return $ a:fromMaybe [] b ++ c:d

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
    c <- loopStmtList
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
    e <- loopStmtList
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
    e <- loopStmtList
    _ <- TT.unindent

    return $ [a] ++ [b] ++ [c] ++ [d] ++ e

parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    -- TODO improve error message
    runParserT stmtList table_stack "Error message" token_list
