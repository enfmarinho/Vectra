{-# LANGUAGE RecordWildCards #-}
module SymbolTable
  ( SymbolType
  , SymbolTableType
  , SymbolTableStackType
  , StateType
  -- , openScope
  -- , closeScope
  -- , updateSymbol
  ) where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy

-- openScope :: StateType () 
-- openScope = do
--     stack <- get
--     table <- liftIO H.new
--     put (table : stack)
--
-- closeScope :: StateType ()
-- closeScope = do
--     (_ : stack) <- get
--     put stack

-- insertSymbol :: SymbolType -> StateType ()
-- insertSymbol (symbol_id, symbol_type) = do
--     a <- consultSymbol symbol_id
--     case a of
--         Nothing -> updateSymbol (symbol_id, symbol_type)
--         Just _ -> fail "symbol already exists"
--
-- updateSymbol :: SymbolType -> StateType ()
-- updateSymbol (symbol_id, symbol_type) = do
--     st@ParserState{..} <- getState
--     case symbolTableStack of
--         [] -> fail "No open scope"
--         (top : rest) -> do
--             liftIO $ H.insert top symbol_id symbol_type
--             putState st { symbolTableStack = top:rest}
--
-- consultSymbol :: String -> StateType (Maybe Type)
-- consultSymbol symbol = do
--     ParserState{..} <- getState
--     case symbolTableStack of
--         [] -> return Nothing
--         -- TODO this is only searching on the top SymbolTable, it should search recursively
--         ((top, _) : _) -> liftIO $ H.lookup top symbol
