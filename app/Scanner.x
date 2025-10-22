{

module Scanner where

import System.IO
import System.IO.Unsafe

}

%wrapper "monadUserState"

$NUMBER = 0-9     
$LETTER = [a-zA-Z]

tokens :-
  "//".*;                                 -- Ignore single line comments
  [\ \n\f\v\r]+;                          -- Ignore white spaces that aren't tabs
  ";"                                     { \p s -> KW_SEMICOLUMN p }
  ":"                                     { \p s -> KW_COLUMN p } 
  "="                                     { \p s -> KW_ASSIGNMENT p }
  $NUMBER+                                { \p s -> INT_LITERAL p (read s) }
  if                                      { \p s -> KW_IF p }
  $LETTER [$LETTER $NUMBER \_]*	          { \p s -> ID p s }
{

data AlexUserState = AlexUserState
  { pastIndentationLevel :: Int
  , currIndentationLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { pastIndentationLevel = 0
  , currIndentationLevel = 0
  }

getPastIndentationLevel :: Alex Int
getPastIndentationLevel = pastIndentationLevel <$> alexGetUserState

setPastIndentationLevel :: Int -> Alex ()
setPastIndentationLevel identation_level = do
  ust <- alexGetUserState
  alexSetUserState ust{ currIndentationLevel = identation_level }

getCurrIndentationLevel :: Alex Int
getCurrIndentationLevel = currIndentationLevel <$> alexGetUserState

setCurrIndentationLevel :: Int -> Alex ()
setCurrIndentationLevel identation_level = do
  ust <- alexGetUserState
  alexSetUserState ust{ currIndentationLevel = identation_level }

alexEOF :: Alex result
alexEOF = do
    -- todo it should emit all the remaining DEDENTs
    ust <- alexGetUserState
    Right(INDENT ust)

data Token =
  ID AlexState String |
  INT_LITERAL AlexState Int |
  KW_SEMICOLUMN AlexState |
  KW_COLUMN AlexState |
  KW_ASSIGNMENT AlexState |
  INDENT AlexState |
  KW_IF AlexState 

getTokens :: FilePath -> IO [Token]
getTokens fn = do
    fh <- openFile fn ReadMode
    s  <- hGetContents fh
    return (alexMonadScan s)

}
