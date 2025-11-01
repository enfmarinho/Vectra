module Assert where

import Scanner
import Text.Parsec
import ParserState
import Types
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- Aux function to emit semantic error messages and finish execution early with err
semanticError :: String -> StateType a
semanticError msg = parserFail ("Semantic Error: " ++ msg)

warningMsg :: String -> StateType ()
warningMsg msg = liftIO $ putStrLn $ "Warning: " ++ msg

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) =
    "(Line " ++ show line ++ ", Column " ++ show col ++ ")"

consultType :: String -> AlexPosn -> StateType Type
consultType symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just t -> return t

assertIterableType :: String -> Type -> AlexPosn -> StateType ()
assertIterableType symbolId t posn = do
    case t of
        ArrayType _ _ -> return ()
        _ -> semanticError $ symbolId ++ " is not iterable " ++ showPos posn

assertBooleanCompatible :: Type -> AlexPosn -> StateType ()
assertBooleanCompatible t posn = do
    case t of
        BoolType -> return ()
        IntType -> return ()
        FloatType -> return ()
        CharType -> return ()
        RefType _ -> return ()
        ConstType ct -> assertBooleanCompatible ct posn
        _ -> semanticError $ show t ++ " cannot be interpreted as a bool " ++ showPos posn

assertAssignableType :: String -> Type -> AlexPosn -> StateType ()
assertAssignableType symbolId t posn = do
    let errMsg = "Trying to assign to " ++ symbolId ++ " which is an non-assignable type: " ++ show t ++ " " ++ showPos posn
    case t of
        ConstType _ -> semanticError errMsg
        ArrayType {} -> semanticError errMsg
        -- TODO FuncType?
        StructType {} -> semanticError errMsg
        _ -> return ()

assertTypesEq :: Type -> Type -> AlexPosn -> StateType ()
assertTypesEq lhs rhs posn = do
    when (lhs /= rhs) 
        $ semanticError $ "Type mismatch between " ++ show lhs ++ " and " ++ show rhs ++ " " ++ showPos posn  

assertValidParamList :: [Type] -> [Type] -> AlexPosn -> StateType ()
assertValidParamList (l:lRest) (r:rRest) posn = do
    when (l /= r) 
        $ semanticError $ "Type mismatch in method call: expected " ++ show r ++ ", but got " ++ show l ++ " " ++ showPos posn
    assertValidParamList lRest rRest posn
assertValidParamList [] (_:_) posn = do
    semanticError $ "Method called with missing arguments " ++ showPos posn
assertValidParamList (_:_) [] posn = do
    semanticError $ "Method called with more arguments than expected " ++ showPos posn
assertValidParamList [] [] _ = do
    return ()

assertNumberTypeReturnInt :: Value -> AlexPosn -> StateType Int
assertNumberTypeReturnInt value posn = do
    case value of
        IntValue intValue -> return intValue
        _ -> semanticError $ "Array size should be declared with a int type " ++ showPos posn

checkShadowing :: String -> AlexPosn -> StateType ()
checkShadowing symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> return ()
        Just _ -> warningMsg $ "Declaring " ++ symbolId ++ " shadows and exists symbol " ++ showPos posn


