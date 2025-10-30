module SymbolTable
  ( SymbolType
  , SymbolTableType
  , SymbolTableStackType
  , StateType
  , openScope
  , closeScope
  , updateSymbol
  ) where

import qualified Data.HashTable.IO as H
import Types
import Text.Parsec
import Control.Monad.State.Lazy

openScope :: SymbolTableStackState () 
openScope = do
    stack <- get
    table <- liftIO H.new
    put (table : stack)

closeScope :: SymbolTableStackState ()
closeScope = do
    (_ : stack) <- get
    put stack

updateSymbol :: SymbolType -> StateType ()
updateSymbol (symbol_id, symbol_type) = do
    stack <- getState
    case stack of
        [] -> fail "No open scope"
        (table : rest) -> do
            liftIO $ H.insert table symbol_id symbol_type
            modifyState (\_ -> table : rest)

consultSymbol :: String -> StateType (Maybe Type)
consultSymbol symbol = do
    stack <- getState
    case stack of
        [] -> return Nothing
        (table : _) -> liftIO $ H.lookup table symbol
