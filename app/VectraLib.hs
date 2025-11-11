module VectraLib where

import InterpreterState
import Types
import Control.Monad.IO.Class
import Assert
import qualified System.IO as IO
import Data.Char (isSpace, toLower)
import Text.Read (readMaybe)

import Scanner
import Control.Monad

importSpecialMethod :: String -> AlexPosn -> StateType ()
importSpecialMethod symbolId posn = do
    case symbolId of
        "print" -> insertSymbol ("print", HaskellMethod [TemplateType] Nothing vectraPrint) True
        "println" -> insertSymbol ("println", HaskellMethod [TemplateType] Nothing vectraPrintln) True
        "read_int" -> insertSymbol ("read_int", HaskellMethod [] (Just IntType) vectraReadInt) True
        "read_float" -> insertSymbol ("read_float", HaskellMethod [] (Just FloatType) vectraReadFloat) True
        "read_bool" -> insertSymbol ("read_bool", HaskellMethod [] (Just BoolType) vectraReadBool) True
        "read_string" -> insertSymbol ("read_string", HaskellMethod [] (Just StringType) vectraReadString) True
        "read_line" -> insertSymbol ("read_line", HaskellMethod [] (Just StringType) vectraReadLine) True
        -- TODO add more methods to stdlib
        _ -> semanticError $ "Invalid import: " ++ symbolId ++ " doesn't exist " ++ showPos posn
    return ()

vectraPrint :: LibMethodSignature
vectraPrint [] posn = semanticError $ "print called without arguments, it requires one " ++ showPos posn
vectraPrint [value] posn = do
    case value of
        IntValue v -> liftIO $ putStr (show v)
        FloatValue v -> liftIO $ putStr (show v)
        CharValue v -> liftIO $ putStr (show v)
        BoolValue v -> liftIO $ putStr (show v)
        StringValue v -> liftIO $ putStr v
        ConstValue v -> do
            _ <- vectraPrint [v] posn -- The return will be Nothing anyway...
            return ()
        _ -> runtimeError $ "Unsupported type to print " ++ showPos posn
    liftIO $ IO.hFlush IO.stdout
    return Nothing
vectraPrint (_:_) posn = semanticError $ "print called with to many arguments " ++ showPos posn


vectraPrintln :: LibMethodSignature
vectraPrintln [] posn = semanticError $ "println called without arguments, it requires one " ++ showPos posn
vectraPrintln [value] posn = do
    _ <- vectraPrint [value] posn
    liftIO $ putStrLn "" -- jump line
    return Nothing
vectraPrintln (_:_) posn = semanticError $ "println called with to many arguments " ++ showPos posn

-- helper function to simulate stdin behaviour
readToken :: IO String
readToken = skipSpaces >> readWord
  where
    skipSpaces = do
        c <- IO.hLookAhead IO.stdin
        when (isSpace c) $ getChar >> skipSpaces

    readWord = do
        c <- IO.hLookAhead IO.stdin
        if isSpace c
            then return []
            else do
                _ <- getChar
                (c :) <$> readWord


vectraReadInt :: LibMethodSignature
vectraReadInt [] _ = do
    tok <- liftIO readToken
    case readMaybe tok :: Maybe Int of
        Just n  -> return (Just $ IntValue n)
        Nothing -> runtimeError $ "Invalid integer input: " ++ tok
vectraReadInt _ posn =
    semanticError $ "read_int called with arguments, but it doesn't take any " ++ showPos posn


vectraReadFloat :: LibMethodSignature
vectraReadFloat [] _ = do
    tok <- liftIO readToken
    case readMaybe tok :: Maybe Float of
        Just f  -> return (Just $ FloatValue f)
        Nothing -> runtimeError $ "Invalid float input: " ++ tok
vectraReadFloat _ posn =
    semanticError $ "read_float called with arguments, but it doesn't take any " ++ showPos posn


vectraReadBool :: LibMethodSignature
vectraReadBool [] _ = do
    tok <- liftIO (fmap (map toLower) readToken)
    case tok of
        "true"  -> return (Just $ BoolValue True)
        "false" -> return (Just $ BoolValue False)
        _       -> runtimeError $ "Invalid boolean input ,expected true or false, but got " ++ tok
vectraReadBool _ posn =
    semanticError $ "read_bool called with arguments, but it doesn't take any " ++ showPos posn


vectraReadString :: LibMethodSignature
vectraReadString [] _ = do
    tok <- liftIO readToken
    return (Just $ StringValue tok)
vectraReadString _ posn =
    semanticError $ "read_string called with arguments, but it doesn't take any " ++ showPos posn


vectraReadLine :: LibMethodSignature
vectraReadLine [] _ = do
    line <- liftIO getLine
    return (Just $ StringValue line)
vectraReadLine _ posn =
    semanticError $ "read_line called with arguments, but it doesn't take any " ++ showPos posn
