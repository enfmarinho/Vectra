module Assert where

import Scanner
import ParserState
import Types
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe

warningMsg :: String -> StateType ()
warningMsg msg = liftIO $ putStrLn $ "Warning: " ++ msg

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) =
    "(Line " ++ show line ++ ", Column " ++ show col ++ ")"

consultTypeList :: String -> AlexPosn -> StateType [Type]
consultTypeList symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just t -> return t

getEnumOrStructTypes :: String -> AlexPosn -> [Type] -> StateType Type
getEnumOrStructTypes symbolId posn (h:t) = do
    case h of
        EnumType list -> return  $ EnumType list
        StructType templateList dataList -> return $ StructType templateList dataList
        _ -> getEnumOrStructTypes symbolId posn t
getEnumOrStructTypes symbolId posn [] = do
    semanticError $ symbolId ++ " should be either an Enum or a Struct " ++ showPos posn


consultType :: String -> AlexPosn -> StateType Type
consultType symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just [] -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just [h] -> return h
        Just (_:_) -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn

assertMethodDeclNotAmbiguous :: String -> [Type] -> AlexPosn -> StateType ()
assertMethodDeclNotAmbiguous symbolId paramTypeList posn = do
    maybeTypeList <- consultSymbol symbolId
    let typeList = fromMaybe [] maybeTypeList

    when (ambiguous typeList paramTypeList) $
        semanticError $
            "Ambiguous declaration for " ++ symbolId ++ " at " ++ showPos posn
  where
    ambiguous :: [Type] -> [Type] -> Bool
    ambiguous [] _ = False
    ambiguous (h:t) paramList =
        let currParamList = case h of
                ProcType _ params _   -> map snd params
                FuncType _ params _ _ -> map snd params
                _                     -> []
         in typeListMatch currParamList paramList || ambiguous t paramList

    typeListMatch :: [Type] -> [Type] -> Bool
    typeListMatch [] [] = True
    typeListMatch [] _  = False
    typeListMatch _  [] = False
    typeListMatch (h1:t1) (h2:t2)
        | h1 /= h2  = False
        | otherwise = typeListMatch t1 t2

assertIterableType :: String -> Type -> AlexPosn -> StateType ()
assertIterableType symbolId t posn = do
    case t of
        ArrayType _ _ -> return ()
        _ -> semanticError $ symbolId ++ " is not iterable " ++ showPos posn

assertStructType :: String -> AlexPosn -> [Type]  -> StateType ()
assertStructType symbolId posn (h:t) = do
    case h of
        StructType {} -> assertStructType symbolId posn t
        ImplNamespaceType {} -> assertStructType symbolId posn t 
        _ -> semanticError $ symbolId ++ " must be a struct " ++ showPos posn
assertStructType _ _ [] = return ()

assertNamespaceType :: String -> Type -> AlexPosn -> StateType ()
assertNamespaceType symbolId t posn = do
    case t of
        NamespaceType {} -> return ()
        ImplNamespaceType {} -> return ()
        _ -> semanticError $ symbolId ++ " must be a namespace " ++ showPos posn

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
        FuncType {} -> semanticError errMsg
        StructType {} -> semanticError errMsg
        _ -> return ()

assertTypesEq :: Type -> Type -> AlexPosn -> StateType ()
assertTypesEq l r posn = do
    when (l /= r)
        $ semanticError $ "Type mismatch between " ++ show l ++ " and " ++ show r ++ " " ++ showPos posn

assertTypeListTypeEq :: [Type] -> Type -> AlexPosn -> StateType ()
assertTypeListTypeEq (l:ltail) r posn = do
    when (l /= r && null ltail)
        $ semanticError $ "Type mismatch between " ++ show l ++ " and " ++ show r ++ " " ++ showPos posn
assertTypeListTypeEq [] _ posn = semanticError $ "Type mismatch "  ++ showPos posn

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

handleAdd :: Value -> Value -> StateType Value
handleAdd lhs _rhs = do
    -- TODO
    return lhs

handleSub :: Value -> Value -> StateType Value
handleSub lhs _rhs = do
    -- TODO
    return lhs

handleMult :: Value -> Value -> StateType Value
handleMult lhs _rhs = do
    -- TODO
    return lhs

handleDiv :: Value -> Value -> StateType Value
handleDiv lhs _rhs = do
    -- TODO
    return lhs

handleAnd :: Value -> Value -> StateType Value
handleAnd lhs _rhs = do
    -- TODO
    return lhs

handleOr :: Value -> Value -> StateType Value
handleOr lhs _rhs = do
    -- TODO
    return lhs

handleNot :: Value -> StateType Value
handleNot lhs = do
    -- TODO
    return lhs
