{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Parser
  ( parser
  ) where

import SymbolTable
import TerminalTokens as TT
import Scanner
import Data.Maybe
import Types
import Text.Parsec
import qualified Text.Parsec as TT

-- program :: Parsec [Token] st [Token]
-- program = do
--             a <- programToken 
--             b <- idToken 
--             c <- beginToken 
--             d <- stmts
--             e <- endToken
--             eof
--             return (a:b:[c] ++ d ++ [e])
--
-- stmts :: Parsec [Token] st [Token]
-- stmts = do
--           first <- assign
--           next <- remaining_stmts
--           return (first ++ next)
--
-- remainingStmts :: Parsec [Token] st [Token]
-- remainingStmts = (do a <- semiColonToken
--                      b <- assign
--                      return (a:b)) <|> return []
--
-- stmtList :: StateType [Token]
-- stmtList = many stmtWithSemi
--   where
--     stmtWithSemi = do
--       s <- stmt
--       _ <- TT.kwSemicolumn
--       return s

-- TODO this is incomplete
expStmt :: StateType [Token]
expStmt = do
    t <- literal
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

assignStmt :: StateType [Token]
assignStmt = do
          a <- TT.id
          b <- TT.kwAssingment
          c <- expStmt
          -- updateSymbol ("id lexema", IntType 1) -- TODO actually update the symbol table correctly
          return (a:b:c)


stmt :: StateType [Token]
stmt = do
    t <- expStmt
      <|> assignStmt
      <|> ifStmt
    return t


-- TODO ignore TT.newLine in the begging
stmtList :: StateType [Token]
stmtList = do
    concat <$> (stmt `sepEndBy1` TT.newLine)


parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    -- TODO improve error message
    runParserT stmtList table_stack "Error message" token_list
