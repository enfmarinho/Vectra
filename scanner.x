{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$NUMBER = 0-9     
$LETTER = [a-zA-Z]

tokens :-
  "//".*;                                 -- Ignore single line comments
  "    "                                  { \p s -> TAB p}
  [\t]                                    { \p s -> TAB p }
  [\ \n\f\v\r]+;                          -- Ignore white spaces that aren't tabs
  ";"                                     { \p s -> KW_SEMICOLUMN p }
  ":"                                     { \p s -> KW_COLUMN p } ","                                     { \p s -> KW_COMMA p }
  "="                                     { \p s -> KW_ASSIGNMENT p }
  "!"                                     { \p s -> OP_NOT p }
  "&&"                                    { \p s -> OP_AND p }
  "||"                                    { \p s -> OP_OR p }
  [\< \> \>= \>= == !=]                   { \p s -> OP_COMPARE p s}
  "("                                     { \p s -> OPEN_PAREN p }
  ")"                                     { \p s -> CLOSE_PAREN p }
  "["                                     { \p s -> OPEN_BRACKET p }
  "]"                                     { \p s -> CLOSE_BRACKET p }
  "+"                                     { \p s -> OP_ADD p }
  "-"                                     { \p s -> OP_SUB p }
  "*"                                     { \p s -> OP_MULT p }
  "/"                                     { \p s -> OP_DIV p }
  "~"                                     { \p s -> KW_TIL p }
  $NUMBER+                                { \p s -> INT_LITERAL p (read s) }
  $NUMBER+\.$NUMBER*                      { \p s -> FLOAT_LITERAL p (read s) }
  \".*\"                                  { \p s -> STRING_LITERAL p s }
  const                                   { \p s -> KW_CONST p }
  int                                     { \p s -> KW_INT p }
  float                                   { \p s -> KW_FLOAT p }
  string                                  { \p s -> KW_STRING p }
  bool                                    { \p s -> KW_BOOL p }
  ref                                     { \p s -> KW_REF p }
  enum                                    { \p s -> KW_ENUM p }
  if                                      { \p s -> KW_IF p }
  else                                    { \p s -> KW_ELSE p }
  while                                   { \p s -> KW_WHILE p }
  for                                     { \p s -> KW_FOR p }
  block                                   { \p s -> KW_BLOCK p }
  public                                  { \p s -> KW_PUBLIC p }
  private                                 { \p s -> KW_PRIVATE p }
  func                                    { \p s -> KW_FUNC p }
  return                                  { \p s -> KW_RETURN p }
  deref                                   { \p s -> KW_DEREF p }
  import                                  { \p s -> KW_IMPORT p }
  false                                   { \p s -> KW_FALSE p }
  true                                    { \p s -> KW_TRUE p }
  $LETTER [$LETTER $NUMBER \_]*	          { \p s -> ID p s }
{

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
  OP_MULT AlexPosn |
  OP_DIV AlexPosn |
  OP_COMPARE AlexPosn String |
  OPEN_PAREN AlexPosn |
  CLOSE_PAREN AlexPosn |
  OPEN_BRACKET AlexPosn |
  CLOSE_BRACKET AlexPosn |
  TAB AlexPosn |
  KW_IF AlexPosn |
  KW_INT AlexPosn |
  KW_FLOAT AlexPosn |
  KW_STRING AlexPosn |
  KW_BOOL AlexPosn |
  KW_FOR AlexPosn |
  KW_REF AlexPosn |
  KW_FUNC AlexPosn |
  KW_ENUM AlexPosn |
  KW_ELSE AlexPosn |
  KW_WHILE AlexPosn |
  KW_DEREF AlexPosn |
  KW_BLOCK AlexPosn |
  KW_CONST AlexPosn |
  KW_TRUE AlexPosn |
  KW_FALSE AlexPosn |
  KW_RETURN AlexPosn |
  KW_IMPORT AlexPosn |
  KW_PUBLIC AlexPosn |
  KW_TIL AlexPosn |
  KW_PRIVATE AlexPosn
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
