{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Utils where

import Scanner
import InterpreterState
import Types
import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (genericLength) -- TODO there are better ways
import Data.Foldable 

warningMsg :: String -> StateType ()
warningMsg msg = liftIO $ putStrLn $ "Warning: " ++ msg

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) =
    "(Line " ++ show line ++ ", Column " ++ show col ++ ")"

getBooleanValue :: Maybe Value -> AlexPosn -> StateType Bool
getBooleanValue Nothing posn = semanticError $ "using uninitialized var " ++ showPos posn
getBooleanValue (Just value) posn = do
    case value of
        BoolValue v -> return v
        IntValue v -> return $ v /= 0
        FloatValue v -> return $ v /= 0
        -- RefValue v -> v /= 0 -- TODO return true in case ref is valid
        ConstValue v -> getBooleanValue (Just v) posn
        _ -> fail $ "Trying to get a bool from something that cannot be interpreted as such " ++ showPos posn -- Should not reach this, since assertBooleanCompatible should be called previously 


getCustomType :: [Type] -> StateType (Maybe Type)
getCustomType (h:t) = do
    case h of
        EnumType name list -> return $ Just $ EnumType name list
        StructType templateList dataTable methodTable -> return $ Just $ StructType templateList dataTable methodTable
        TemplateType s -> return $ Just $ TemplateType s
        _ -> getCustomType t
getCustomType [] = return Nothing


getStructType :: [Type] -> StateType (Maybe Type)
getStructType (h:t) = do
    case h of
        StructType templateList dataTable methodTable -> return $ Just $ StructType templateList dataTable methodTable
        _ -> getStructType t
getStructType [] = return Nothing


consultType :: String -> AlexPosn -> StateType Type
consultType symbolId posn = do
    consultResult <- consultSymbolTable symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        -- improve error message
        Just ([], _) -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just ([h], _) -> return h
        Just (_:_, _) -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn


consultTypeList :: String -> AlexPosn -> StateType [Type]
consultTypeList symbolId posn = do
    consultResult <- consultSymbolTable symbolId
    case consultResult of
        Nothing -> semanticError $ symbolId ++ " doesn't exist in this scope " ++ showPos posn
        Just (t, _) -> return t


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

castType :: Type -> Type -> AlexPosn -> StateType Type

-- IntType target
castType IntType IntType _ = return IntType
castType IntType FloatType _ = return IntType
castType IntType BoolType _ = return IntType
castType IntType StringType _ = return IntType
castType IntType (ConstType srcT) posn = castType IntType srcT posn

-- FloatType target
castType FloatType IntType _ = return FloatType
castType FloatType FloatType _ = return FloatType
castType FloatType BoolType _ = return FloatType
castType FloatType StringType _ = return FloatType
castType FloatType (ConstType srcT) posn = castType FloatType srcT posn

-- BoolType target
castType BoolType BoolType _ = return BoolType
castType BoolType IntType _ = return BoolType
castType BoolType FloatType _ = return BoolType
castType BoolType (ConstType srcT) posn = castType BoolType srcT posn

-- StringType target
castType StringType StringType _ = return StringType
castType StringType IntType _ = return StringType
castType StringType FloatType _ = return StringType
castType StringType BoolType _ = return StringType
castType StringType (ConstType srcT) posn = castType StringType srcT posn

-- Const Target
castType (ConstType targetT) srcT posn = do
    result <- castType targetT srcT posn
    return (ConstType result)

-- ArrayType target
castType (ArrayType t1) (ArrayType t2) posn = do
    finalT <- castType t1 t2 posn
    return (ArrayType finalT)

-- EnumType target
castType (EnumType name1 labels1) (EnumType name2 labels2) posn
    | EnumType name1 labels1 == EnumType name2 labels2 = return (EnumType name1 labels1)
    | otherwise =
        semanticError $ "Incompatible enum types at " ++ showPos posn

castType target src posn =
    semanticError $ "Cannot cast from " ++ show src ++ " to " ++ show target ++ " at " ++ showPos posn


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

castValueToType (ArrayType _)(_, ArrayValue array) _ = return (ArrayValue array)

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

resultOpType _ _ posn = semanticError $ "<resultOpType> " ++ showPos posn

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
        _ -> fail "<searchTypeList>" -- should not reach this

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

typeFromValue :: Value -> AlexPosn -> StateType Type
typeFromValue (IntValue _) _ = return IntType
typeFromValue (FloatValue _) _ = return FloatType
typeFromValue (CharValue _) _ = return CharType
typeFromValue (BoolValue _) _ = return BoolType
typeFromValue (StringValue _) _ = return StringType
typeFromValue (ConstValue v) posn = do
    t <- typeFromValue v posn
    return (ConstType t)

typeFromValue (ArrayValue _) _ = return $ ArrayType $ TemplateType Nothing

typeFromValue (EnumValue _) _ = return (EnumLabelType "")
typeFromValue (RefValue symbolId _) posn = do
    symbolType <- consultType symbolId posn
    return (RefType symbolType)

-- Function/procedure references
typeFromValue (FuncRefValue symbolId) posn = do
    t <- consultType symbolId posn
    case t of
        FuncType templates paramPairs ret _ -> return (FuncRefType templates (map snd paramPairs) ret)
        _ -> semanticError $ "Invalid function reference type for symbol " ++ symbolId

typeFromValue (ProcRefValue symbolId) posn = do
    t <- consultType symbolId posn
    case t of
        ProcType templates paramPairs _ -> return (ProcRefType templates (map snd paramPairs))
        _ -> semanticError $ "Invalid procedure reference type for symbol " ++ symbolId

typeFromValue (StructValue _symbolTable) _ = return (StructInstanceType "")

-- This don't check for ambiguities, it should have already been done
mergeTablesInPlace :: SymbolTableType -> SymbolTableType -> IO ()
mergeTablesInPlace destiny source = do
    pairs <- liftIO $ H.toList source
    forM_ pairs $ \(k, (s, v)) -> do
        existing <- liftIO $ H.lookup destiny k
        case existing of
            Nothing -> liftIO $ H.insert destiny k (s, v)
            Just (existingList, _) -> do
                liftIO $ H.insert destiny k (existingList ++ s, Nothing)

searchTypeOnTable :: SymbolTableType -> String -> StateType (Maybe Type)
searchTypeOnTable table symbolId = do
    result <- liftIO $ H.lookup table symbolId
    case result of
        Nothing -> return Nothing
        Just (tList, _) -> do
            t <- getTypeFromTypeList tList
            return $ Just t


searchTypeOnStruct :: SymbolTableType -> SymbolTableType -> [String] -> StateType (Maybe Type)
searchTypeOnStruct publicTable privateTable [symbolListH] = do
    result <- searchTypeOnTable publicTable symbolListH
    case result of 
        Nothing -> searchTypeOnTable privateTable symbolListH
        Just t -> return $ Just t
searchTypeOnStruct publicTable privateTable (symbolListH:symbolListT) = do
    a <- searchTypeOnStruct publicTable privateTable [symbolListH]
    case a of 
        Nothing -> semanticError $ "member " ++ "\"" ++ symbolListH ++ "\"" ++ " doesn't exist on this context"
        Just t -> case t of
                    StructType _ p1 p2 -> searchTypeOnStruct p1 p2 symbolListT
                    _ -> semanticError $ "member " ++ "\"" ++ symbolListH ++ "\"" ++ " is not a struct type"
searchTypeOnStruct _ _ [] = return Nothing
