module VectraLib where

import InterpreterState
import Types
import Control.Monad.IO.Class
import Assert
import Scanner

importSpecialMethod :: String -> AlexPosn -> StateType ()
importSpecialMethod symbolId posn = do
    case symbolId of
        "print" -> insertSymbol ("print", HaskellMethod [TemplateType] Nothing vectraPrint) True 
        "println" -> insertSymbol ("println", HaskellMethod [TemplateType] Nothing vectraPrintln) True 
        -- TODO add more methods to stdlib
        _ -> semanticError $ "Invalid import: " ++ symbolId ++ "doesn't exist " ++ showPos posn
    return ()

importFile :: String -> AlexPosn -> StateType ()
importFile _filePath _posn = do
    -- TODO
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
        _ -> semanticError $ "Unsupported type on method call " ++ showPos posn
    return Nothing
vectraPrint (_:_) posn = semanticError $ "print called with to many arguments " ++ showPos posn


vectraPrintln :: LibMethodSignature
vectraPrintln [] posn = semanticError $ "println called without arguments, it requires one " ++ showPos posn
vectraPrintln [value] posn = do
    _ <- vectraPrint [value] posn
    liftIO $ putStrLn "" -- jump line
    return Nothing
vectraPrintln (_:_) posn = semanticError $ "println called with to many arguments " ++ showPos posn
