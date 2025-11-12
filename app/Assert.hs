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
    consultResult <- consultSymbolTable symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just t -> return t


assertNonAmbiguous :: String -> AlexPosn -> StateType ()
assertNonAmbiguous symbolId posn = do
    a <- consultSymbolTable symbolId 
    case a of
        Nothing -> return ()
        Just _ -> semanticError $ "Ambiguous declaration for symbol " ++ symbolId ++ " " ++ showPos posn


getEnumOrStructTypes :: [Type] -> StateType (Maybe Type)
getEnumOrStructTypes (h:t) = do
    case h of
        EnumType list -> return $ Just $ EnumType list
        StructType templateList dataList -> return $ Just $ StructType templateList dataList
        _ -> getEnumOrStructTypes t
getEnumOrStructTypes [] = return Nothing


consultType :: String -> AlexPosn -> StateType Type
consultType symbolId posn = do
    consultResult <- consultSymbolTable symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        -- improve error message
        Just [] -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just [h] -> return h
        Just (_:_) -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn

assertMethodDeclNotAmbiguous :: String -> [Type] -> AlexPosn -> StateType ()
assertMethodDeclNotAmbiguous symbolId paramTypeList posn = do
    maybeTypeList <- consultSymbolTable symbolId
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
        ArrayType _ -> return ()
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

getBooleanValue :: Maybe Value -> AlexPosn -> StateType Bool
getBooleanValue Nothing posn = semanticError $ "TODO " ++ showPos posn
getBooleanValue (Just value) posn = do
    case value of
        BoolValue v -> return v
        IntValue v -> return $ v /= 0
        FloatValue v -> return $ v /= 0
        -- RefValue v -> v /= 0 -- TODO return true in case ref is valid
        ConstValue v -> getBooleanValue (Just v) posn
        _ -> fail $ "Trying to get a bool from something that cannot be interpreted as such " ++ showPos posn -- Should not reach this, since assertBooleanCompatible should be called previously 

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

assertInBounds :: String -> Int -> Int -> AlexPosn -> StateType ()
assertInBounds symbolId size idx posn = do
    unless (idx >= 0 && idx < size) $ runtimeError
        $ "Trying access index " ++ show idx ++ " of " ++ symbolId ++ " but it's size is " ++ show size ++ showPos posn

checkShadowing :: String -> AlexPosn -> StateType ()
checkShadowing symbolId posn = do
    consultResult <- consultSymbolTable symbolId
    case consultResult of
        Nothing -> return ()
        Just _ -> warningMsg $ "Declaring " ++ symbolId ++ " shadows and exists symbol " ++ showPos posn


toBoolValue :: Value -> Value
toBoolValue (BoolValue b) = BoolValue b
toBoolValue (IntValue i) = BoolValue (i /= 0)
toBoolValue (FloatValue f) = BoolValue (f /= 0)
toBoolValue (ConstValue v) = toBoolValue v
toBoolValue _ = BoolValue False  -- Fallback for unsupported types


handleNot :: Maybe Value -> AlexPosn -> StateType (Type, Value)
handleNot Nothing posn = semanticError $ "Invalid operand for logical '!' " ++ showPos posn
handleNot (Just (ConstValue v)) posn = handleNot (Just v) posn
handleNot (Just (BoolValue v)) _ = return (BoolType, BoolValue (not v))
handleNot (Just (IntValue v)) _ = return (BoolType, BoolValue (v == 0))
handleNot (Just (FloatValue v)) _ = return (BoolType, BoolValue (v == 0.0))
handleNot _ posn = semanticError $ "Invalid operand for logical '!' " ++ showPos posn


handleAnd :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleAnd Nothing _ posn = semanticError $ "Invalid operands for logical '&&' " ++ showPos posn
handleAnd _ Nothing posn = semanticError $ "Invalid operands for logical '&&' " ++ showPos posn
handleAnd (Just (ConstValue lhs)) rhs posn = handleAnd (Just lhs) rhs posn
handleAnd lhs (Just (ConstValue rhs)) posn = handleAnd lhs (Just rhs) posn
handleAnd (Just lhs) (Just rhs) _ = do
    let BoolValue lhsB = toBoolValue lhs
        BoolValue rhsB = toBoolValue rhs
    return (BoolType, BoolValue (lhsB && rhsB))


handleOr :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleOr Nothing _ posn = semanticError $ "Invalid operands for logical '||' " ++ showPos posn
handleOr _ Nothing posn = semanticError $ "Invalid operands for logical '||' " ++ showPos posn
handleOr (Just (ConstValue lhs)) rhs posn = handleOr (Just lhs) rhs posn
handleOr lhs (Just (ConstValue rhs)) posn = handleOr lhs (Just rhs) posn
handleOr (Just lhs) (Just rhs) _ = do
    let BoolValue lhsB = toBoolValue lhs
        BoolValue rhsB = toBoolValue rhs
    return (BoolType, BoolValue (lhsB || rhsB))


handleUnaryMinus :: Maybe Value -> AlexPosn -> StateType (Type, Value)
handleUnaryMinus Nothing posn = semanticError $ "Invalid minus unary operation " ++ showPos posn
handleUnaryMinus (Just (ConstValue v)) posn = handleUnaryMinus (Just v) posn
handleUnaryMinus (Just (IntValue v)) _ = return (IntType, IntValue (-v))
handleUnaryMinus (Just (FloatValue v)) _ = return (FloatType, FloatValue (-v))
handleUnaryMinus _ posn = semanticError $ "Invalid minus unary operation " ++ showPos posn

handleComparison :: Maybe Value -> Maybe Value -> Token -> AlexPosn -> StateType (Type, Value)
handleComparison Nothing _ _ posn = semanticError $ "Invalid operands for comparision at " ++ showPos posn
handleComparison _ Nothing _ posn = semanticError $ "Invalid operands for comparision at " ++ showPos posn
-- TODO missing handleComparison implementation, for example StructValue
handleComparison (Just (StringValue lhs)) (Just (StringValue rhs)) compOp _ =
    case compOp of
        OP_EQ _     -> return (BoolType, BoolValue $ lhs == rhs)
        OP_NOT_EQ _ -> return (BoolType, BoolValue $ lhs /= rhs)
        _ -> semanticError "Invalid operator for strings"
handleComparison (Just lhsV) (Just rhsV) compOp posn = do
    lhsT <- typeFromValue lhsV posn
    lhsV' <- castValueToType FloatType (lhsT, lhsV) posn
    let FloatValue lhs = lhsV'

    rhsT <- typeFromValue rhsV posn
    rhsV' <- castValueToType FloatType (rhsT, rhsV) posn
    let FloatValue rhs = rhsV'

    case compOp of
        OP_SMALLER _ -> return (BoolType, BoolValue $ lhs < rhs)
        OP_SMALLER_EQ _ -> return (BoolType, BoolValue $ lhs <= rhs) 
        OP_GREATER _ -> return (BoolType, BoolValue $ lhs > rhs) 
        OP_GREATER_EQ _ -> return (BoolType, BoolValue $ lhs >= rhs) 
        OP_EQ _ -> return (BoolType, BoolValue $ lhs == rhs) 
        OP_NOT_EQ _ -> return (BoolType, BoolValue $ lhs /= rhs) 
        _ -> fail "<handleComparison>"


handleAdd :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleAdd Nothing _ posn = semanticError $ "Invalid operands for addition at " ++ showPos posn
handleAdd _ Nothing posn = semanticError $ "Invalid operands for addition at " ++ showPos posn
handleAdd (Just (ConstValue lhs)) rhs posn = handleAdd (Just lhs) rhs posn
handleAdd lhs (Just (ConstValue rhs)) posn = handleAdd lhs (Just rhs) posn
handleAdd (Just (IntValue lhsV)) (Just (IntValue rhsV)) _ =
    return (IntType, IntValue (lhsV + rhsV))
handleAdd (Just (IntValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (fromIntegral lhsV + rhsV))
handleAdd (Just (FloatValue lhsV)) (Just (IntValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV + fromIntegral rhsV))
handleAdd (Just (FloatValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV + rhsV))
handleAdd (Just (StringValue lhsV)) (Just (StringValue rhsV)) _ =
    return (StringType, StringValue (lhsV ++ rhsV))
handleAdd (Just (StringValue lhsV)) (Just (CharValue rhsV)) _ =
    return (StringType, StringValue (lhsV ++ [rhsV]))
handleAdd _ _ posn = semanticError $ "Invalid operands for addition at " ++ showPos posn


handleSub :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleSub Nothing _ posn = semanticError $ "Invalid operands for subtraction at " ++ showPos posn
handleSub _ Nothing posn = semanticError $ "Invalid operands for subtraction at " ++ showPos posn
handleSub (Just (ConstValue lhs)) rhs posn = handleSub (Just lhs) rhs posn
handleSub lhs (Just (ConstValue rhs)) posn = handleSub lhs (Just rhs) posn
handleSub (Just (IntValue lhsV)) (Just (IntValue rhsV)) _ =
    return (IntType, IntValue (lhsV - rhsV))
handleSub (Just (IntValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (fromIntegral lhsV - rhsV))
handleSub (Just (FloatValue lhsV)) (Just (IntValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV - fromIntegral rhsV))
handleSub (Just (FloatValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV - rhsV))
handleSub _ _ posn = semanticError $ "Invalid operands for subtraction at " ++ showPos posn


handleMult :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleMult Nothing _ posn =
    semanticError $ "Left operand of multiplication doesn't have a value, maybe it wasn't initialized " ++ showPos posn
handleMult _ Nothing posn =
    semanticError $ "Right operand of multiplication is missing at " ++ showPos posn
handleMult (Just (ConstValue lhs)) rhs posn =
    handleMult (Just lhs) rhs posn
handleMult lhs (Just (ConstValue rhs)) posn =
    handleMult lhs (Just rhs) posn
handleMult (Just (IntValue lhsV)) (Just (IntValue rhsV)) _ =
    return (IntType, IntValue (lhsV * rhsV))
handleMult (Just (IntValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (fromIntegral lhsV * rhsV))
handleMult (Just (FloatValue lhsV)) (Just (IntValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV * fromIntegral rhsV))
handleMult (Just (FloatValue lhsV)) (Just (FloatValue rhsV)) _ =
    return (FloatType, FloatValue (lhsV * rhsV))
handleMult _ _ posn =
    semanticError $ "Invalid operands for multiplication at " ++ showPos posn


handleDiv :: Maybe Value -> Maybe Value -> AlexPosn -> StateType (Type, Value)
handleDiv Nothing _ posn = semanticError $ "Invalid operands for division at " ++ showPos posn
handleDiv _ Nothing posn = semanticError $ "Invalid operands for division at " ++ showPos posn
handleDiv (Just (ConstValue lhs)) rhs posn = handleDiv (Just lhs) rhs posn
handleDiv lhs (Just (ConstValue rhs)) posn = handleDiv lhs (Just rhs) posn
handleDiv (Just (IntValue lhsV)) (Just (IntValue rhsV)) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (fromIntegral lhsV / fromIntegral rhsV))
handleDiv (Just (IntValue lhsV)) (Just (FloatValue rhsV)) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (fromIntegral lhsV / rhsV))
handleDiv (Just (FloatValue lhsV)) (Just (IntValue rhsV)) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (lhsV / fromIntegral rhsV))
handleDiv (Just (FloatValue lhsV)) (Just (FloatValue rhsV)) posn =
    if rhsV == 0
        then semanticError $ "Division by zero at " ++ showPos posn
        else return (FloatType, FloatValue (lhsV / rhsV))
handleDiv _ _ posn = semanticError $ "Invalid operands for division at " ++ showPos posn


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

getTypeFromTypeList :: [Type] -> StateType Type
getTypeFromTypeList [h] = return h
getTypeFromTypeList [] = fail "<getTypeFromTypeList> empty list"
getTypeFromTypeList _ = fail "<getTypeFromTypeList> ambiguity"
