module Assert where

import Scanner
import Text.Parsec
import ParserState
import Types
import Control.Monad (when)

consultType :: String -> AlexPosn -> StateType Type
consultType symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> semanticError "TODO errmsg2"
        Just t -> return t

assertTemplateCorrectInstanciation :: [String] -> [Type] -> AlexPosn -> StateType ()
assertTemplateCorrectInstanciation templatesIds templatesTypes posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

assertArrayType :: Type -> AlexPosn -> StateType ()
assertArrayType t posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

assertBooleanType :: Type -> AlexPosn -> StateType ()
assertBooleanType t posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

assertAssignableType :: Type -> AlexPosn -> StateType ()
assertAssignableType t posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

assertTypesEq :: Type -> Type -> AlexPosn -> StateType ()
assertTypesEq lhs rhs posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

assertTypesMatch :: [Type] -> [Type] -> AlexPosn -> StateType ()
assertTypesMatch lhs rhs posn = do
    -- TODO implement this
    Control.Monad.when False $ semanticError "asd"

-- Aux function to emit semantic error messages and finish execution early with err
semanticError :: String -> StateType a
semanticError msg = parserFail ("Semantic Error: " ++ msg)

