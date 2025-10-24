{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Parser
  ( parser
  ) where

import SymbolTable
import TerminalTokens as TT
import Scanner
import Data.Maybe
import Text.Parsec


paramList :: StateType [Token]
paramList = do
    concat <$> (varDeclStmt `sepEndBy1` TT.kwComma)

callStmt :: StateType [Token]
callStmt = do
    a <- TT.id
    _ <- TT.openParen
    b <- paramList
    _ <- TT.closeParen
    -- TODO assure that exists a function called id and that there is no type error
    return $ a:b

-- TODO better name this ? 
expDecl :: StateType [Token]
expDecl = do
    t <- literal 
        <|> typeDecl
    return t


literal :: StateType [Token]
literal = do
    t <- TT.intLiteral
      <|> TT.floatLiteral
      <|> TT.stringLiteral
      <|> TT.kwTrue
      <|> TT.kwFalse
    return [t]


ifStmt :: StateType [Token]
ifStmt = do
    a <- TT.kwIf
    b <- expDecl
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

assignStmt :: StateType [Token]
assignStmt = do
          a <- TT.id
          b <- TT.kwAssingment
          c <- expDecl
          -- updateSymbol ("id lexema", IntType 1) -- TODO actually update the symbol table correctly
          return (a:b:c)

whileStmt :: StateType [Token]
whileStmt = do
    a <- TT.kwWhile
    b <- expDecl
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    c <- stmtList
    _ <- TT.unindent
    return (a:b ++ c)


-- TODO better name this ?
typeDecl :: StateType [Token]
typeDecl = do
    a <- (:[]) <$> TT.kwInt
        <|> (:[]) <$> TT.kwFloat
        <|> (:[]) <$> TT.kwBool
        <|> (:[]) <$> TT.kwString
        <|> do -- refType
            a <- TT.kwRef
            _ <- TT.openParen
            b <- typeDecl
            _ <- TT.closeParen
            return (a : b)
        <|> do -- customType
            a <- TT.id
            -- TODO check if id is a valid type, i.e. if there is a block declaration that matches id in the symbol table
            return [a]
    return a


varDeclStmt :: StateType [Token]
varDeclStmt = do
    a <- typeDecl
    -- TODO will this cause problems considering that assignStmt also consumes a TT.id at first ? 
    -- maybe one of those shift-reduce errors ? I don't think it will, since i believe it will go down the route that 
    -- consumes more tokens but I'm unsure about it
    b <- assignStmt
      <|> (:[]) <$> TT.id

    return $ a ++ b

forStmt :: StateType [Token]
forStmt = do
    a <- TT.kwFor
    b <- optionMaybe varDeclStmt
    _ <- TT.kwSemicolumn
    c <- optionMaybe expDecl
    _ <- TT.kwSemicolumn
    d <- optionMaybe expDecl
    _ <- TT.kwColumn
    _ <- TT.newLine
    _ <- TT.indent
    e <- stmtList
    _ <- TT.unindent

    return (a : fromMaybe [] b ++ fromMaybe [] c ++ fromMaybe [] d ++ e)

-- TODO incomplete: missing some unimplemented stmt rules
stmt :: StateType [Token]
stmt = do
    t <- assignStmt
      <|> ifStmt
      <|> whileStmt
      <|> forStmt
    return t

stmtList :: StateType [Token]
stmtList = do
    _ <- optionMaybe TT.newLine
    concat <$> (stmt `sepEndBy1` TT.newLine)


parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    -- TODO improve error message
    runParserT stmtList table_stack "Error message" token_list
