{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Assert where

import Scanner
import InterpreterState
import Types
import Control.Monad (when, unless)
import Data.Maybe

assertNonAmbiguous :: String -> AlexPosn -> StateType ()
assertNonAmbiguous symbolId posn = do
    a <- consultSymbolMaybe symbolId
    when (isJust a) $ semanticError $ "Ambiguous declaration for \"" ++ symbolId ++ "\" " ++ showPos posn


assertMethodDeclNotAmbiguous :: String -> [Type] -> AlexPosn -> StateType ()
assertMethodDeclNotAmbiguous symbolId paramTypeList posn = do
    maybeTypeList <- consultSymbolMaybe symbolId
    typeList <- case maybeTypeList of
                    Nothing -> return []
                    Just (t, _) -> return t

    when (ambiguous typeList paramTypeList) $
        semanticError $
            "Ambiguous declaration for subprogram \"" ++ symbolId ++ "\" " ++ showPos posn
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
    case currProgramState of
        Loop _ -> return ()
        _ -> semanticError $ "Trying to use break outside a loop " ++ showPos posn


assertContinuable :: AlexPosn -> StateType ()
assertContinuable posn = do
    currProgramState <- getParserBlock
    case currProgramState of
        Loop _ -> return ()
        _ -> semanticError $ "Trying to use continue outside a loop " ++ showPos posn


assertReturnType :: Maybe Type -> AlexPosn -> StateType ()
assertReturnType maybeReturnT posn = do
    s <- getParserBlock
    expectedMaybeT <- case s of
                Method maybeT -> return maybeT 
                Loop maybeT -> return maybeT
                _ -> semanticError $ "return statement outside a method " ++ showPos posn

    case maybeReturnT of
        Nothing -> when (isJust expectedMaybeT) $ semanticError "Missing return value"
        Just returnT -> do
            case expectedMaybeT of
                Nothing -> semanticError $ "returning a value inside a procedure " ++ showPos posn
                Just expectedT -> assertTypesEq returnT expectedT posn


assertCustomType :: Type -> AlexPosn -> StateType ()
assertCustomType t posn = do
    case t of
        StructType {} -> return ()
        EnumLabelType {} -> return ()
        _ -> semanticError $ "invalid type " ++ show t ++ " " ++ showPos posn


assertComparableTypes :: Type -> Type -> AlexPosn -> StateType ()
assertComparableTypes _t1 _t2 _posn = return () -- TODO

assertIterableType :: String -> Type -> AlexPosn -> StateType ()
assertIterableType symbolId t posn = do
    case t of
        ArrayType _ -> return ()
        _ -> semanticError $ symbolId ++ " is not iterable " ++ showPos posn


assertStructType :: String -> AlexPosn -> [Type]  -> StateType ()
assertStructType symbolId posn (h:t) = do
    case h of
        StructType {} -> assertStructType symbolId posn t
        ImplType {} -> assertStructType symbolId posn t
        _ -> semanticError $ symbolId ++ " must be a struct " ++ showPos posn
assertStructType _ _ [] = return ()


assertBooleanCompatible :: Type -> AlexPosn -> StateType ()
assertBooleanCompatible t posn = do
    case t of
        BoolType -> return ()
        IntType -> return ()
        FloatType -> return ()
        RefType _ -> return ()
        ConstType ct -> assertBooleanCompatible ct posn
        _ -> semanticError $ show t ++ " cannot be interpreted as a bool " ++ showPos posn


assertAssignableType :: String -> Type -> AlexPosn -> StateType ()
assertAssignableType symbolId t posn = do
    let errMsg = "Trying to assign to " ++ symbolId ++ " which is an non-assignable type: " ++ show t ++ " " ++ showPos posn
    case t of
        ConstType _ -> semanticError errMsg
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

