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

-- remaining_stmts :: Parsec [Token] st [Token]
-- remaining_stmts = (do a <- semiColonToken
--                       b <- assign
--                       return (a:b)) <|> (return [])

-- assignStmt :: Parsec [Token] st [Token]
-- assignStmt :: ParserType [Token]
assignStmt :: StateType [Token]
assignStmt = do
          a <- TT.id
          b <- TT.kwAssingment
          c <- TT.intLiteral
          updateSymbol ("a", IntType 1)
          return (a:b:[c])

parser :: [Token] -> SymbolTableStackType -> IO (Either ParseError [Token])
parser token_list table_stack = do
    runParserT assignStmt table_stack "Error message" token_list 
