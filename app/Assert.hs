{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Assert where

import Scanner
import InterpreterState
import Types
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (genericLength)
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
        -- improve error message
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

assertBreakable :: AlexPosn -> StateType ()
assertBreakable posn = do
    currProgramState <- getParserBlock
    when (currProgramState /= Loop && currProgramState /= Conditional)
        $ semanticError $ "Trying to use break outside a loop " ++ showPos posn

assertContinuable :: AlexPosn -> StateType ()
assertContinuable posn = do
    currProgramState <- getParserBlock
    when (currProgramState /= Loop)
        $ semanticError $ "Trying to use continue outside a loop " ++ showPos posn

assertReturnable :: AlexPosn -> StateType ()
assertReturnable posn = do
    currProgramState <- getParserBlock
    when (currProgramState == GlobalScope) $ semanticError $ "Trying to use return outside a method " ++ showPos posn

assertReturnType :: Type -> AlexPosn -> StateType ()
assertReturnType returnT posn = do
    s <- getParserBlock
    case s of
        Method maybeT -> case maybeT of
                        Nothing -> semanticError $ "returning a value inside a procedure " ++ showPos posn
                        Just t -> assertTypesEq returnT t posn
        _ -> semanticError $ "return statement outside a method " ++ showPos posn

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
        RefType _ -> return ()
        ConstType ct -> assertBooleanCompatible ct posn
        _ -> semanticError $ show t ++ " cannot be interpreted as a bool " ++ showPos posn

getBooleanValue :: Value -> StateType Bool 
getBooleanValue value = do
    case value of 
        BoolValue v -> return v
        IntValue v -> return $ v /= 0
        FloatValue v -> return $ v /= 0
        -- RefValue v -> v /= 0 -- TODO return true in case ref is valid
        ConstValue v -> getBooleanValue v
        _ -> fail "Trying to get a bool from something that cannot be interpreted as such" -- Should not reach this, since assertBooleanCompatible should be called previously 

assertAssignableType :: String -> Type -> AlexPosn -> StateType ()
assertAssignableType symbolId t posn = do
    let errMsg = "Trying to assign to " ++ symbolId ++ " which is an non-assignable type: " ++ show t ++ " " ++ showPos posn
    case t of
        ConstType _ -> semanticError errMsg
        ArrayType {} -> semanticError errMsg
        EnumType {} -> semanticError errMsg
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

assertInBounds :: String -> Int -> Int -> AlexPosn -> StateType ()
assertInBounds symbolId size idx posn = do
    unless (idx >= 0 && idx < size) $ runtimeError
        $ "Trying access index " ++ show idx ++ " of " ++ symbolId ++ " but it's size is " ++ show size ++ showPos posn

checkShadowing :: String -> AlexPosn -> StateType ()
checkShadowing symbolId posn = do
    consultResult <- consultSymbol symbolId
    case consultResult of
        Nothing -> return ()
        Just _ -> warningMsg $ "Declaring " ++ symbolId ++ " shadows and exists symbol " ++ showPos posn


toBoolValue :: Value -> Value
toBoolValue (BoolValue b) = BoolValue b
toBoolValue (IntValue i) = BoolValue (i /= 0)
toBoolValue (FloatValue f) = BoolValue (f /= 0)
toBoolValue (ConstValue v) = toBoolValue v
toBoolValue _ = BoolValue False  -- Fallback for unsupported types


handleNot :: Value -> AlexPosn -> StateType (Type, Value)
handleNot (ConstValue v) posn = handleNot v posn
handleNot (BoolValue v) _ = return (BoolType, BoolValue (not v))
handleNot (IntValue v) _ = return (BoolType, BoolValue (v == 0))
handleNot (FloatValue v) _ = return (BoolType, BoolValue (v == 0.0))
handleNot _ posn = semanticError $ "Invalid operand for logical '!' " ++ showPos posn


handleAnd :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleAnd (ConstValue lhs) rhs posn = handleAnd lhs rhs posn
handleAnd lhs (ConstValue rhs) posn = handleAnd lhs rhs posn
handleAnd lhs rhs _ = do
    let BoolValue lhsB = toBoolValue lhs
        BoolValue rhsB = toBoolValue rhs
    return (BoolType, BoolValue (lhsB && rhsB))


handleOr :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleOr (ConstValue lhs) rhs posn = handleOr lhs rhs posn
handleOr lhs (ConstValue rhs) posn = handleOr lhs rhs posn
handleOr lhs rhs _ = do
    let BoolValue lhsB = toBoolValue lhs
        BoolValue rhsB = toBoolValue rhs
    return (BoolType, BoolValue (lhsB || rhsB))


handleUnaryMinus :: Value -> AlexPosn -> StateType (Type, Value)
handleUnaryMinus (ConstValue v) posn = handleUnaryMinus v posn
handleUnaryMinus (IntValue v) _ = return (IntType, IntValue (-v))
handleUnaryMinus (FloatValue v) _ = return (FloatType, FloatValue (-v))
handleUnaryMinus _ posn = semanticError $ "Invalid minus unary operation " ++ showPos posn


handleAdd :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleAdd (ConstValue lhs) rhs posn = handleAdd lhs rhs posn
handleAdd lhs (ConstValue rhs) posn = handleAdd lhs rhs posn
handleAdd (IntValue lhsV) (IntValue rhsV) _ =
    return (IntType, IntValue (lhsV + rhsV))
handleAdd (IntValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (fromIntegral lhsV + rhsV))
handleAdd (FloatValue lhsV) (IntValue rhsV) _ =
    return (FloatType, FloatValue (lhsV + fromIntegral rhsV))
handleAdd (FloatValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (lhsV + rhsV))
handleAdd (StringValue lhsV) (StringValue rhsV) _ =
    return (StringType, StringValue (lhsV ++ rhsV))
handleAdd _ _ posn =
    semanticError $ "Invalid operands for addition at " ++ showPos posn


handleSub :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleSub (ConstValue lhs) rhs posn = handleSub lhs rhs posn
handleSub lhs (ConstValue rhs) posn = handleSub lhs rhs posn
handleSub (IntValue lhsV) (IntValue rhsV) _ =
    return (IntType, IntValue (lhsV - rhsV))
handleSub (IntValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (fromIntegral lhsV - rhsV))
handleSub (FloatValue lhsV) (IntValue rhsV) _ =
    return (FloatType, FloatValue (lhsV - fromIntegral rhsV))
handleSub (FloatValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (lhsV - rhsV))
handleSub _ _ posn =
    semanticError $ "Invalid operands for subtraction at " ++ showPos posn


handleMult :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleMult (ConstValue lhs) rhs posn = handleMult lhs rhs posn
handleMult lhs (ConstValue rhs) posn = handleMult lhs rhs posn
handleMult (IntValue lhsV) (IntValue rhsV) _ =
    return (IntType, IntValue (lhsV * rhsV))
handleMult (IntValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (fromIntegral lhsV * rhsV))
handleMult (FloatValue lhsV) (IntValue rhsV) _ =
    return (FloatType, FloatValue (lhsV * fromIntegral rhsV))
handleMult (FloatValue lhsV) (FloatValue rhsV) _ =
    return (FloatType, FloatValue (lhsV * rhsV))
handleMult _ _ posn =
    semanticError $ "Invalid operands for multiplication at " ++ showPos posn


handleDiv :: Value -> Value -> AlexPosn -> StateType (Type, Value)
handleDiv (ConstValue lhs) rhs posn = handleDiv lhs rhs posn
handleDiv lhs (ConstValue rhs) posn = handleDiv lhs rhs posn
handleDiv (IntValue lhsV) (IntValue rhsV) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (fromIntegral lhsV / fromIntegral rhsV))
handleDiv (IntValue lhsV) (FloatValue rhsV) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (fromIntegral lhsV / rhsV))
handleDiv (FloatValue lhsV) (IntValue rhsV) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (lhsV / fromIntegral rhsV))
handleDiv (FloatValue lhsV) (FloatValue rhsV) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (lhsV / rhsV))
handleDiv _ _ posn =
    semanticError $ "Invalid operands for division at " ++ showPos posn

castValueToType :: Type -> (Type, Value) -> AlexPosn -> StateType Value
--- IntType target ---
castValueToType IntType (_, IntValue i) _ = return (IntValue i)
castValueToType IntType (_, FloatValue f) _ = return (IntValue (floor f))
castValueToType IntType (_, BoolValue b) _ = return (IntValue (if b then 1 else 0))
castValueToType IntType (_, StringValue s) posn =
    case reads s :: [(Int, String)] of
        [(i, "")] -> return (IntValue i)
        _         -> runtimeError $
                        "unsuccessful casting between " ++ show IntType ++ " and " ++ show StringType ++
                        ". Not possible to get an int from this string " ++ showPos posn
castValueToType IntType (srcT, ConstValue v) posn =
    castValueToType IntType (srcT, v) posn

--- FloatType target ---
castValueToType FloatType (_, IntValue i) _ = return (FloatValue (fromIntegral i))
castValueToType FloatType (_, FloatValue f) _ = return (FloatValue f)
castValueToType FloatType (_, BoolValue b) _ = return (FloatValue (if b then 1.0 else 0.0))
castValueToType FloatType (_, StringValue s) posn =
    case reads s :: [(Float, String)] of
        [(f, "")] -> return (FloatValue f)
        _         -> runtimeError $
                        "unsuccessful casting between " ++ show FloatType ++ " and " ++ show StringType ++
                        ". Not possible to get a float from this string " ++ showPos posn
castValueToType FloatType (srcT, ConstValue v) posn =
    castValueToType FloatType (srcT, v) posn

--- BoolType target ---
castValueToType BoolType (_, BoolValue b) _ = return (BoolValue b)
castValueToType BoolType (_, IntValue i) _ = return (BoolValue (i /= 0))
castValueToType BoolType (_, FloatValue f) _ = return (BoolValue (f /= 0))
castValueToType BoolType (srcT, ConstValue v) posn =
    castValueToType BoolType (srcT, v) posn

--- StringType target ---
castValueToType StringType (_, StringValue s) _ = return (StringValue s)
castValueToType StringType (_, IntValue i) _ = return (StringValue (show i))
castValueToType StringType (_, FloatValue f) _ = return (StringValue (show f))
castValueToType StringType (_, BoolValue b) _ = return (StringValue (show b))
castValueToType StringType (srcT, ConstValue v) posn =
    castValueToType StringType (srcT, v) posn

-- TODO what about TemplateType ? 
--- Unsupported cast ---
castValueToType targetType (srcT, _) posn =
    semanticError $
        "Invalid cast operation, casting between incompatible types: " ++
        show targetType ++ " and " ++ show srcT ++ " " ++ showPos posn

resultOpType :: Type -> Type -> AlexPosn -> StateType Type
-- Int
resultOpType IntType IntType _ = return IntType
resultOpType IntType FloatType _ = return FloatType
resultOpType IntType BoolType _ = return IntType
-- Float 
resultOpType FloatType FloatType _ = return FloatType
resultOpType FloatType IntType _ = return FloatType
resultOpType FloatType BoolType _ = return FloatType
-- String 
resultOpType StringType StringType _ = return StringType
resultOpType StringType CharType _ = return StringType
-- Bool
resultOpType BoolType BoolType _ = return BoolType
resultOpType BoolType IntType _ = return IntType
resultOpType BoolType FloatType _ = return FloatType
-- Const
resultOpType (ConstType lhs) rhs posn = resultOpType lhs rhs posn
resultOpType lhs (ConstType rhs) posn = resultOpType lhs rhs posn

resultOpType _ _ posn = semanticError $ "TODO resultOpType " ++ showPos posn

searchTypeList :: [Type] -> Int -> [Type] -> StateType (Maybe Type)
searchTypeList [] _ _ = return Nothing
searchTypeList (typesH:typesT) templateLen typeList = do
    (templateList, expectedTypeList) <- case typesH of
        ProcType templateList expectedParamList _ -> do
            let (_, expectedTypeList) = unzip expectedParamList
            return (templateList, expectedTypeList)
        FuncType templateList expectedParamList _ _ -> do
            let (_, expectedTypeList) = unzip expectedParamList
            return (templateList, expectedTypeList)
        HaskellMethod expectedTypeList _ _ -> return ([], expectedTypeList)
        _ -> fail "what????" -- TODO

    let expectedTemplateLen = genericLength templateList
    if expectedTemplateLen == templateLen && typeListMatch expectedTypeList typeList 
        then return $ Just typesH
        else searchTypeList typesT templateLen typeList
    where
        typeListMatch :: [Type] -> [Type] -> Bool
        typeListMatch [] [] = True
        typeListMatch _ [] = False
        typeListMatch [] _ = False
        typeListMatch (lhsH:lhsT) (rhsH:rhsT) = do
            (lhsH == rhsH) && typeListMatch lhsT rhsT
