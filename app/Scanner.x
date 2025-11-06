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
    "//".*       { \_ _ -> return Nothing }
    [\ \t]+      { \_ len -> do setCurrIndentationLevel len; return Nothing }
    [\n]+        { \aInp _ -> do setBeginLine True; setCurrIndentationLevel 0; return $ Just (NEWLINE (alexPos aInp))}

    -- Punctuation and operators
    ";"          { \aInp _ -> do t <- handleIndentation (KW_SEMICOLUMN (alexPos aInp)); return $ Just t }
    "::"         { \aInp _ -> do t <- handleIndentation (KW_DOUBLE_COLUMN (alexPos aInp)); return $ Just t }
    ":"          { \aInp _ -> do t <- handleIndentation (KW_COLUMN (alexPos aInp)); return $ Just t }
    ","          { \aInp _ -> do t <- handleIndentation (KW_COMMA (alexPos aInp)); return $ Just t }
    "."          { \aInp _ -> do t <- handleIndentation (KW_DOT (alexPos aInp)); return $ Just t }
    "="          { \aInp _ -> do t <- handleIndentation (KW_ASSIGNMENT (alexPos aInp)); return $ Just t }
    "!"          { \aInp _ -> do t <- handleIndentation (OP_NOT (alexPos aInp)); return $ Just t }
    "&&"         { \aInp _ -> do t <- handleIndentation (OP_AND (alexPos aInp)); return $ Just t }
    "||"         { \aInp _ -> do t <- handleIndentation (OP_OR (alexPos aInp)); return $ Just t }
    \<           { \aInp _ -> do t <- handleIndentation (OP_SMALLER (alexPos aInp)); return $ Just t }
    \>           { \aInp _ -> do t <- handleIndentation (OP_GREATER (alexPos aInp)); return $ Just t }
    \<=          { \aInp _ -> do t <- handleIndentation (OP_SMALLER_EQ (alexPos aInp)); return $ Just t }
    \>=          { \aInp _ -> do t <- handleIndentation (OP_GREATER_EQ (alexPos aInp)); return $ Just t }
    ==           { \aInp _ -> do t <- handleIndentation (OP_EQ (alexPos aInp)); return $ Just t }
    !=           { \aInp _ -> do t <- handleIndentation (OP_NOT_EQ (alexPos aInp)); return $ Just t }
    "("          { \aInp _ -> do t <- handleIndentation (OPEN_PAREN (alexPos aInp)); return $ Just t }
    ")"          { \aInp _ -> do t <- handleIndentation (CLOSE_PAREN (alexPos aInp)); return $ Just t }
    "["          { \aInp _ -> do t <- handleIndentation (OPEN_BRACKET (alexPos aInp)); return $ Just t }
    "]"          { \aInp _ -> do t <- handleIndentation (CLOSE_BRACKET (alexPos aInp)); return $ Just t }
    "{"          { \aInp _ -> do t <- handleIndentation (OPEN_CURLY (alexPos aInp)); return $ Just t }
    "}"          { \aInp _ -> do t <- handleIndentation (CLOSE_CURLY (alexPos aInp)); return $ Just t }
    "+"          { \aInp _ -> do t <- handleIndentation (OP_ADD (alexPos aInp)); return $ Just t }
    "-"          { \aInp _ -> do t <- handleIndentation (OP_SUB (alexPos aInp)); return $ Just t }
    "*"          { \aInp _ -> do t <- handleIndentation (OP_MULT (alexPos aInp)); return $ Just t }
    "/"          { \aInp _ -> do t <- handleIndentation (OP_DIV (alexPos aInp)); return $ Just t }
    "~"          { \aInp _ -> do t <- handleIndentation (KW_TIL (alexPos aInp)); return $ Just t }

    -- Literals
    $NUMBER+     { \aInp len -> do t <- handleIndentation (INT_LITERAL (alexPos aInp) (read (take len (alexInputStr aInp)))); return $ Just t }
    $NUMBER+\.$NUMBER* { \aInp len -> do t <- handleIndentation (FLOAT_LITERAL (alexPos aInp) (read (take len (alexInputStr aInp)))); return $ Just t }
    \".*\"       { \aInp len -> do t <- handleIndentation (STRING_LITERAL (alexPos aInp) (take len (alexInputStr aInp))); return $ Just t }

    -- Keywords
    namespace    { \aInp _ -> do t <- handleIndentation (KW_NAMESPACE (alexPos aInp)); return $ Just t }
    const        { \aInp _ -> do t <- handleIndentation (KW_CONST (alexPos aInp)); return $ Just t }
    cast         { \aInp _ -> do t <- handleIndentation (KW_CAST (alexPos aInp)); return $ Just t }
    impl         { \aInp _ -> do t <- handleIndentation (KW_IMPL (alexPos aInp)); return $ Just t }
    local        { \aInp _ -> do t <- handleIndentation (KW_LOCAL (alexPos aInp)); return $ Just t }
    int          { \aInp _ -> do t <- handleIndentation (KW_INT (alexPos aInp)); return $ Just t }
    float        { \aInp _ -> do t <- handleIndentation (KW_FLOAT (alexPos aInp)); return $ Just t }
    string       { \aInp _ -> do t <- handleIndentation (KW_STRING (alexPos aInp)); return $ Just t }
    bool         { \aInp _ -> do t <- handleIndentation (KW_BOOL (alexPos aInp)); return $ Just t }
    ref          { \aInp _ -> do t <- handleIndentation (KW_REF (alexPos aInp)); return $ Just t }
    enum         { \aInp _ -> do t <- handleIndentation (KW_ENUM (alexPos aInp)); return $ Just t }
    if           { \aInp _ -> do t <- handleIndentation (KW_IF (alexPos aInp)); return $ Just t }
    else         { \aInp _ -> do t <- handleIndentation (KW_ELSE (alexPos aInp)); return $ Just t }
    while        { \aInp _ -> do t <- handleIndentation (KW_WHILE (alexPos aInp)); return $ Just t }
    for          { \aInp _ -> do t <- handleIndentation (KW_FOR (alexPos aInp)); return $ Just t }
    foreach      { \aInp _ -> do t <- handleIndentation (KW_FOREACH (alexPos aInp)); return $ Just t }
    in           { \aInp _ -> do t <- handleIndentation (KW_IN (alexPos aInp)); return $ Just t }
    break        { \aInp _ -> do t <- handleIndentation (KW_BREAK (alexPos aInp)); return $ Just t }
    continue     { \aInp _ -> do t <- handleIndentation (KW_CONTINUE (alexPos aInp)); return $ Just t }
    struct       { \aInp _ -> do t <- handleIndentation (KW_STRUCT (alexPos aInp)); return $ Just t }
    public       { \aInp _ -> do t <- handleIndentation (KW_PUBLIC (alexPos aInp)); return $ Just t }
    private      { \aInp _ -> do t <- handleIndentation (KW_PRIVATE (alexPos aInp)); return $ Just t }
    func         { \aInp _ -> do t <- handleIndentation (KW_FUNC (alexPos aInp)); return $ Just t }
    proc         { \aInp _ -> do t <- handleIndentation (KW_PROC (alexPos aInp)); return $ Just t }
    return       { \aInp _ -> do t <- handleIndentation (KW_RETURN (alexPos aInp)); return $ Just t }
    deref        { \aInp _ -> do t <- handleIndentation (KW_DEREF (alexPos aInp)); return $ Just t }
    import       { \aInp _ -> do t <- handleIndentation (KW_IMPORT (alexPos aInp)); return $ Just t }
    false        { \aInp _ -> do t <- handleIndentation (KW_FALSE (alexPos aInp)); return $ Just t }
    true         { \aInp _ -> do t <- handleIndentation (KW_TRUE (alexPos aInp)); return $ Just t }

    -- IDs
    $LETTER [$LETTER $NUMBER _]* { \aInp len -> do
        t <- handleIndentation (ID (alexPos aInp) (take len (alexInputStr aInp)))
        return (Just t)
    }

{

data AlexUserState = AlexUserState
  { currIndentationLevel :: Int
  , beginLine :: Bool
  , indentationLevelStack :: [Int]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { currIndentationLevel = 0, beginLine = True, indentationLevelStack = [0] }

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

getIndentationLevelStack :: Alex [Int]
getIndentationLevelStack = indentationLevelStack <$> alexGetUserState

topIndentationLevelStack :: Alex Int 
topIndentationLevelStack = do
  stack <- getIndentationLevelStack
  case stack of
    (x:_) -> return x
    []    -> alexError "TODO write this error message"

pushIndentationLevel :: Int -> Alex ()
pushIndentationLevel lvl = do
  ust <- alexGetUserState
  let newStack = lvl : indentationLevelStack ust
  alexSetUserState ust { indentationLevelStack = newStack }

popIndentationLevel :: Alex ()
popIndentationLevel = do
  ust <- alexGetUserState
  case indentationLevelStack ust of
    (_:rest) -> alexSetUserState ust { indentationLevelStack = rest }
    []       -> alexError "TODO write this error message"

alexEOF :: Alex (Maybe Token)
alexEOF = do
  inp <- alexGetInput
  let posn = alexPos inp
  stack <- getIndentationLevelStack
  unindents <- emitUnindents stack posn
  return $ Just (EOF unindents)
  where
    emitUnindents :: [Int] -> AlexPosn -> Alex [Token]
    emitUnindents [] _ = return []
    emitUnindents [_] _ = return []  -- mantém o nível base
    emitUnindents (_:rest) posn = do
        more <- emitUnindents rest posn
        return ((UNINDENT posn): more)

alexPos :: AlexInput -> AlexPosn
alexPos (pos, _, _, _) = pos

alexInputStr :: AlexInput -> String
alexInputStr (_, _, _, str) = str

handleIndentation :: Token -> Alex Token
handleIndentation currToken = do
    pastIndentationLevel <- topIndentationLevelStack
    currIndentationLevel <- getCurrIndentationLevel

    beginLine <- getBeginLine
    setBeginLine False

    inp <- alexGetInput
    let posn = alexPos inp
    if not beginLine then
        return currToken
    else if pastIndentationLevel < currIndentationLevel then do
        pushIndentationLevel currIndentationLevel
        return $ SPECIAL_CASE [INDENT posn, currToken]
    else if pastIndentationLevel > currIndentationLevel then do
        unindents <- unindentLoop pastIndentationLevel currIndentationLevel posn
        return $ SPECIAL_CASE (unindents ++ [NEWLINE posn, currToken]) -- This is a workaround
    else
        return currToken

    where
      unindentLoop :: Int -> Int -> AlexPosn -> Alex [Token]
      unindentLoop past curr posn =
        case compare past curr of
          LT -> alexError "Indentation error: unindent does not match any outer indentation level"
          EQ -> return []
          GT -> do
            popIndentationLevel
            t <- topIndentationLevelStack
            rest <- unindentLoop t curr posn
            return ((UNINDENT posn): rest)


data Token =
  ID AlexPosn String |
  INT_LITERAL AlexPosn Int |
  FLOAT_LITERAL AlexPosn Float |
  STRING_LITERAL AlexPosn String |
  KW_SEMICOLUMN AlexPosn |
  KW_COLUMN AlexPosn |
  KW_DOUBLE_COLUMN AlexPosn |
  KW_COMMA AlexPosn |
  KW_DOT AlexPosn |
  KW_ASSIGNMENT AlexPosn |
  OP_NOT AlexPosn |
  OP_AND AlexPosn |
  OP_OR AlexPosn |
  OP_ADD AlexPosn |
  OP_SUB AlexPosn |
  OP_MULT AlexPosn |
  OP_DIV AlexPosn |
  OP_SMALLER AlexPosn |
  OP_GREATER AlexPosn |
  OP_SMALLER_EQ AlexPosn |
  OP_GREATER_EQ AlexPosn |
  OP_EQ AlexPosn |
  OP_NOT_EQ AlexPosn |
  OPEN_PAREN AlexPosn |
  CLOSE_PAREN AlexPosn |
  OPEN_BRACKET AlexPosn |
  CLOSE_BRACKET AlexPosn |
  OPEN_CURLY AlexPosn |
  CLOSE_CURLY AlexPosn |
  INDENT AlexPosn |
  UNINDENT AlexPosn |
  KW_NAMESPACE AlexPosn |
  KW_IF AlexPosn |
  KW_INT AlexPosn |
  KW_FLOAT AlexPosn |
  KW_STRING AlexPosn |
  KW_BOOL AlexPosn |
  KW_FOR AlexPosn |
  KW_FOREACH AlexPosn |
  KW_IN AlexPosn |
  KW_WHILE AlexPosn |
  KW_BREAK AlexPosn |
  KW_CONTINUE AlexPosn |
  KW_REF AlexPosn |
  KW_DEREF AlexPosn |
  KW_FUNC AlexPosn |
  KW_PROC AlexPosn |
  KW_ENUM AlexPosn |
  KW_ELSE AlexPosn |
  KW_STRUCT AlexPosn |
  KW_CAST AlexPosn |
  KW_CONST AlexPosn |
  KW_IMPL AlexPosn |
  KW_LOCAL AlexPosn |
  KW_TRUE AlexPosn |
  KW_FALSE AlexPosn |
  KW_RETURN AlexPosn |
  KW_IMPORT AlexPosn |
  KW_PUBLIC AlexPosn |
  KW_PRIVATE AlexPosn |
  NEWLINE AlexPosn |
  SPECIAL_CASE [Token] |
  EOF [Token] |
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
                     EOF ts -> return (reverse (ts ++ acc))
                     SPECIAL_CASE ts -> loop (reverse ts ++ acc)
                     _ -> loop (t : acc)


}
