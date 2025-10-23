{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Parser
  ( parser
  ) where

import SymbolTable
import TerminalTokens as TT
import Scanner
import Types
import Text.Parsec

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
      <|> (:[]) <$> TT.kwReturn -- TODO this is wrong, just be an example of usage
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
    return ([a] ++ b ++ [c] ++ d)


assignStmt :: StateType [Token]
assignStmt = do
          a <- TT.id
          b <- TT.kwAssingment
          c <- expStmt
          -- TODO update symbol table
          return (a:b:c)


stmt :: StateType [Token]
stmt = do
    t <- expStmt
      <|> assignStmt
      <|> ifStmt
    return t


stmtList :: StateType [Token]
stmtList = do
    concat <$> (stmt `sepEndBy1` TT.newLine)


parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    -- TODO improve error message
    runParserT stmtList table_stack "Error message" token_list 
