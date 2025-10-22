{

module Scanner where

import System.IO
import System.IO.Unsafe
import Debug.Trace (trace)

}

%wrapper "monadUserState"

$NUMBER = 0-9     
$LETTER = [a-zA-Z]

tokens :-
  "//".*                                  { \_ _ -> return Nothing }
  [\ \t]+                                 { \_ len -> do
                                                        setCurrIndentationLevel len
                                                        return Nothing }
  [\n]+                                   { \_ _ -> do
                                                        setBeginLine True
                                                        setCurrIndentationLevel 0
                                                        return Nothing }
  ";"                                     { \aInp _ -> return $ Just (KW_SEMICOLUMN (alexPos aInp)) }
  ":"                                     { \aInp _ -> return $ Just (KW_COLUMN (alexPos aInp)) } 
  ","                                     { \aInp _ -> return $ Just (KW_COMMA (alexPos aInp)) }
  "="                                     { \aInp _ -> return $ Just (KW_ASSIGNMENT (alexPos aInp)) }
  "!"                                     { \aInp _ -> return $ Just (OP_NOT (alexPos aInp)) }
  "&&"                                    { \aInp _ -> return $ Just (OP_AND (alexPos aInp)) }
  "||"                                    { \aInp _ -> return $ Just (OP_OR (alexPos aInp)) }
  [\< \> \>= \>= == !=]                   { \aInp len -> return $ Just (OP_COMPARE (alexPos aInp) (take len (alexInputStr aInp))) }
  "("                                     { \aInp _ -> return $ Just (OPEN_PAREN (alexPos aInp)) }
  ")"                                     { \aInp _ -> return $ Just (CLOSE_PAREN (alexPos aInp)) }
  "["                                     { \aInp _ -> return $ Just (OPEN_BRACKET (alexPos aInp)) }
  "]"                                     { \aInp _ -> return $ Just (CLOSE_BRACKET (alexPos aInp)) }
  "+"                                     { \aInp _ -> return $ Just (OP_ADD (alexPos aInp)) }
  "-"                                     { \aInp _ -> return $ Just (OP_SUB (alexPos aInp)) }
  "*"                                     { \aInp _ -> return $ Just (OP_MULT (alexPos aInp)) }
  "/"                                     { \aInp _ -> return $ Just (OP_DIV (alexPos aInp)) }
  "~"                                     { \aInp _ -> return $ Just (KW_TIL (alexPos aInp)) }
  $NUMBER+                                { \aInp len -> return $ Just (INT_LITERAL (alexPos aInp) (read (take len (alexInputStr aInp)))) }
  $NUMBER+\.$NUMBER*                      { \aInp len -> return $ Just (FLOAT_LITERAL (alexPos aInp) (read (take len (alexInputStr aInp)))) }
  \".*\"                                  { \aInp len -> return $ Just (STRING_LITERAL (alexPos aInp) (take len (alexInputStr aInp))) }
  const                                   { \aInp _ -> return $ Just (KW_CONST (alexPos aInp)) }
  int                                     { \aInp _ -> return $ Just (KW_INT (alexPos aInp)) }
  float                                   { \aInp _ -> return $ Just (KW_FLOAT (alexPos aInp)) }
  string                                  { \aInp _ -> return $ Just (KW_STRING (alexPos aInp)) }
  bool                                    { \aInp _ -> return $ Just (KW_BOOL (alexPos aInp)) }
  ref                                     { \aInp _ -> return $ Just (KW_REF (alexPos aInp)) }
  enum                                    { \aInp _ -> return $ Just (KW_ENUM (alexPos aInp)) }
  if                                      { \aInp _ -> return $ Just (KW_IF (alexPos aInp)) }
  else                                    { \aInp _ -> return $ Just (KW_ELSE (alexPos aInp)) }
  while                                   { \aInp _ -> return $ Just (KW_WHILE (alexPos aInp)) }
  for                                     { \aInp _ -> return $ Just (KW_FOR (alexPos aInp)) }
  block                                   { \aInp _ -> return $ Just (KW_BLOCK (alexPos aInp)) }
  public                                  { \aInp _ -> return $ Just (KW_PUBLIC (alexPos aInp)) }
  private                                 { \aInp _ -> return $ Just (KW_PRIVATE (alexPos aInp)) }
  func                                    { \aInp _ -> return $ Just (KW_FUNC (alexPos aInp)) }
  return                                  { \aInp _ -> return $ Just (KW_RETURN (alexPos aInp)) }
  deref                                   { \aInp _ -> return $ Just (KW_DEREF (alexPos aInp)) }
  import                                  { \aInp _ -> return $ Just (KW_IMPORT (alexPos aInp)) }
  false                                   { \aInp _ -> return $ Just (KW_FALSE (alexPos aInp)) }
  true                                    { \aInp _ -> return $ Just (KW_TRUE (alexPos aInp)) }
  $LETTER [$LETTER $NUMBER \_]*	          { 
      \aInp len -> do 
        t <- handleIndentation (ID (alexPos aInp) (take len(alexInputStr aInp)))
        return (Just t)
      }
{

data AlexUserState = AlexUserState
  { pastIndentationLevel :: Int
  , currIndentationLevel :: Int
  , beginLine :: Bool
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { pastIndentationLevel = 0, currIndentationLevel = 0, beginLine = True }

getPastIndentationLevel :: Alex Int
getPastIndentationLevel = pastIndentationLevel <$> alexGetUserState

setPastIndentationLevel :: Int -> Alex ()
setPastIndentationLevel lvl = do
  ust <- alexGetUserState
  alexSetUserState ust{ pastIndentationLevel = lvl }

getCurrIndentationLevel :: Alex Int
getCurrIndentationLevel = currIndentationLevel <$> alexGetUserState

setCurrIndentationLevel :: Int -> Alex ()
setCurrIndentationLevel lvl = do
  ust <- alexGetUserState
  alexSetUserState ust{ currIndentationLevel = lvl }

getBeginLine :: Alex Bool
getBeginLine = beginLine <$> alexGetUserState

setBeginLine :: Bool -> Alex ()
setBeginLine b = do
  ust <- alexGetUserState
  alexSetUserState ust{ beginLine = b }

alexEOF :: Alex (Maybe Token)
alexEOF = return $ Just (EOF)

alexPos :: AlexInput -> AlexPosn
alexPos (pos, _, _, _) = pos

alexInputStr :: AlexInput -> String
alexInputStr (_, _, _, str) = str

handleIndentation :: Token -> Alex Token
handleIndentation token = do
    pastIndentationLevel <- getPastIndentationLevel
    currIndentationLevel <- getCurrIndentationLevel
    setPastIndentationLevel currIndentationLevel

    let _ = trace ("pastIndentationLevel = " ++ show pastIndentationLevel) ()

    beginLine <- getBeginLine
    if beginLine then
        return token
    else do
        setBeginLine False
        return $ case compare pastIndentationLevel currIndentationLevel of
            LT -> SPECIAL_CASE [INDENT, token]
            GT -> SPECIAL_CASE [DEDENT, token]
            EQ -> token


data Token =
  ID AlexPosn String |
  INT_LITERAL AlexPosn Int |
  FLOAT_LITERAL AlexPosn Float |
  STRING_LITERAL AlexPosn String |
  KW_SEMICOLUMN AlexPosn |
  KW_COLUMN AlexPosn |
  KW_COMMA AlexPosn |
  KW_ASSIGNMENT AlexPosn |
  OP_NOT AlexPosn |
  OP_AND AlexPosn |
  OP_OR AlexPosn |
  OP_ADD AlexPosn |
  OP_SUB AlexPosn |
  Ignore |
  OP_MULT AlexPosn |
  OP_DIV AlexPosn |
  OP_COMPARE AlexPosn String |
  OPEN_PAREN AlexPosn |
  CLOSE_PAREN AlexPosn |
  OPEN_BRACKET AlexPosn |
  CLOSE_BRACKET AlexPosn |
  INDENT |
  DEDENT |
  KW_IF AlexPosn |
  KW_INT AlexPosn |
  KW_FLOAT AlexPosn |
  KW_STRING AlexPosn |
  KW_BOOL AlexPosn |
  KW_FOR AlexPosn |
  KW_WHILE AlexPosn |
  KW_REF AlexPosn |
  KW_DEREF AlexPosn |
  KW_FUNC AlexPosn |
  KW_ENUM AlexPosn |
  KW_ELSE AlexPosn |
  KW_BLOCK AlexPosn |
  KW_CONST AlexPosn |
  KW_TRUE AlexPosn |
  KW_FALSE AlexPosn |
  KW_RETURN AlexPosn |
  KW_IMPORT AlexPosn |
  KW_PUBLIC AlexPosn |
  KW_PRIVATE AlexPosn |
  SPECIAL_CASE [Token] |
  EOF |
  KW_TIL AlexPosn 
  deriving (Eq,Show)


getTokens :: FilePath -> IO (Either String [Token])
getTokens fn = do
  src <- readFile fn
  return $ runAlex src (loop [])
  where
    loop acc = do
      mtok <- alexMonadScan
      case mtok of
        Nothing -> loop (acc)
        Just t  -> case t of 
                     EOF -> return (reverse acc)
                     _ -> loop (t:acc)


}
