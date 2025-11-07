module VectraLib where

import InterpreterState
import Types
import Control.Monad.IO.Class

vectraPrint :: [Value] -> StateType (Maybe Value)
vectraPrint [] = do
    liftIO $ putStrLn "<print: no arguments>"
    return Nothing
vectraPrint [value] = do
    case value of
        IntValue v -> liftIO $ putStr (show v)
        FloatValue v -> liftIO $ putStr (show v)
        CharValue v -> liftIO $ putStr (show v)
        BoolValue v -> liftIO $ putStr (show v)
        StringValue v -> liftIO $ putStr v
        ConstValue v -> do 
            _ <- vectraPrint [v] -- The return will be Nothing anyway...
            return ()
        _ -> runtimeError "Unsupported type in print"
    return Nothing
vectraPrint (_:_) = runtimeError "print called with to many arguments"


vectraPrintln :: [Value] -> StateType (Maybe Value)
vectraPrintln [] = do
    liftIO $ putStrLn "<print: no arguments>"
    return Nothing
vectraPrintln list = do
    _ <- vectraPrint list
    liftIO $ putStrLn "" -- jump line
    return Nothing
