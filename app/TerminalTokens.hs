module TerminalTokens where

import Scanner
import Text.Parsec
import ParserState

id :: StateType Token
id = tokenPrim show updatePos getToken where
  getToken (ID pos s) = Just (ID pos s)
  getToken _      = Nothing

intLiteral :: StateType Token 
intLiteral = tokenPrim show updatePos getToken where
  getToken (INT_LITERAL pos x) = Just (INT_LITERAL pos x)
  getToken _       = Nothing

floatLiteral :: StateType Token
floatLiteral = tokenPrim show updatePos getToken where
  getToken (FLOAT_LITERAL pos x) = Just (FLOAT_LITERAL pos x)
  getToken _       = Nothing

stringLiteral :: StateType Token
stringLiteral = tokenPrim show updatePos getToken where
  getToken (STRING_LITERAL pos s) = Just (STRING_LITERAL pos s)
  getToken _       = Nothing

kwNamespace :: StateType Token
kwNamespace = tokenPrim show updatePos getToken where
  getToken (KW_NAMESPACE pos) = Just (KW_NAMESPACE pos)
  getToken _       = Nothing

kwSemicolumn :: StateType Token
kwSemicolumn = tokenPrim show updatePos getToken where
  getToken (KW_SEMICOLUMN pos) = Just (KW_SEMICOLUMN pos)
  getToken _       = Nothing

kwColumn :: StateType Token
kwColumn = tokenPrim show updatePos getToken where
  getToken (KW_COLUMN pos) = Just (KW_COLUMN pos)
  getToken _       = Nothing

kwDoubleColumn :: StateType Token
kwDoubleColumn = tokenPrim show updatePos getToken where
  getToken (KW_DOUBLE_COLUMN pos) = Just (KW_DOUBLE_COLUMN pos)
  getToken _       = Nothing

kwComma :: StateType Token
kwComma = tokenPrim show updatePos getToken where
  getToken (KW_COMMA pos) = Just (KW_COMMA pos)
  getToken _       = Nothing

kwDot :: StateType Token
kwDot = tokenPrim show updatePos getToken where
  getToken (KW_DOT pos) = Just (KW_DOT pos)
  getToken _       = Nothing

kwAssingment :: StateType Token
kwAssingment = tokenPrim show updatePos getToken where
  getToken (KW_ASSIGNMENT pos) = Just (KW_ASSIGNMENT pos)
  getToken _       = Nothing

opNot :: StateType Token
opNot = tokenPrim show updatePos getToken where
  getToken (OP_NOT pos) = Just (OP_NOT pos)
  getToken _       = Nothing

opAnd :: StateType Token
opAnd = tokenPrim show updatePos getToken where
  getToken (OP_AND pos) = Just (OP_AND pos)
  getToken _       = Nothing

opOr :: StateType Token
opOr = tokenPrim show updatePos getToken where
  getToken (OP_OR pos) = Just (OP_OR pos)
  getToken _       = Nothing

opAdd :: StateType Token
opAdd = tokenPrim show updatePos getToken where
  getToken (OP_ADD pos) = Just (OP_ADD pos)
  getToken _       = Nothing

opSub :: StateType Token
opSub = tokenPrim show updatePos getToken where
  getToken (OP_SUB pos) = Just (OP_SUB pos)
  getToken _       = Nothing

opMult :: StateType Token
opMult = tokenPrim show updatePos getToken where
  getToken (OP_MULT pos) = Just (OP_MULT pos)
  getToken _       = Nothing

opDiv :: StateType Token
opDiv = tokenPrim show updatePos getToken where
  getToken (OP_DIV pos) = Just (OP_DIV pos)
  getToken _       = Nothing

opSmaller :: StateType Token
opSmaller = tokenPrim show updatePos getToken where
  getToken (OP_SMALLER pos) = Just (OP_SMALLER pos)
  getToken _       = Nothing

opGreater :: StateType Token
opGreater = tokenPrim show updatePos getToken where
  getToken (OP_GREATER pos) = Just (OP_GREATER pos)
  getToken _       = Nothing

opSmallerEq :: StateType Token
opSmallerEq = tokenPrim show updatePos getToken where
  getToken (OP_SMALLER_EQ pos) = Just (OP_SMALLER_EQ pos)
  getToken _       = Nothing

opGreaterEq :: StateType Token
opGreaterEq = tokenPrim show updatePos getToken where
  getToken (OP_GREATER_EQ pos) = Just (OP_GREATER_EQ pos)
  getToken _       = Nothing

opEq :: StateType Token
opEq = tokenPrim show updatePos getToken where
  getToken (OP_EQ pos) = Just (OP_EQ pos)
  getToken _       = Nothing

opNotEq :: StateType Token
opNotEq = tokenPrim show updatePos getToken where
  getToken (OP_NOT_EQ pos) = Just (OP_NOT_EQ pos)
  getToken _       = Nothing

openParen :: StateType Token
openParen = tokenPrim show updatePos getToken where
  getToken (OPEN_PAREN pos) = Just (OPEN_PAREN pos)
  getToken _       = Nothing

closeParen :: StateType Token
closeParen = tokenPrim show updatePos getToken where
  getToken (CLOSE_PAREN pos) = Just (CLOSE_PAREN pos)
  getToken _       = Nothing

openBracket :: StateType Token
openBracket = tokenPrim show updatePos getToken where
  getToken (OPEN_BRACKET pos) = Just (OPEN_BRACKET pos)
  getToken _       = Nothing

closeBracket :: StateType Token
closeBracket = tokenPrim show updatePos getToken where
  getToken (CLOSE_BRACKET pos) = Just (CLOSE_BRACKET pos)
  getToken _       = Nothing

indent :: StateType Token
indent = tokenPrim show updatePos getToken where
  getToken (INDENT posn) = Just (INDENT posn)
  getToken _       = Nothing

unindent :: StateType Token
unindent = tokenPrim show updatePos getToken where
  getToken (UNINDENT posn) = Just (UNINDENT posn)
  getToken _       = Nothing

newLine :: StateType Token
newLine = tokenPrim show updatePos getToken where
  getToken (NEWLINE pos) = Just (NEWLINE pos)
  getToken _       = Nothing

kwIf :: StateType Token
kwIf = tokenPrim show updatePos getToken where
  getToken (KW_IF pos) = Just (KW_IF pos)
  getToken _       = Nothing

kwInt :: StateType Token
kwInt = tokenPrim show updatePos getToken where
  getToken (KW_INT pos) = Just (KW_INT pos)
  getToken _       = Nothing

kwFloat :: StateType Token
kwFloat = tokenPrim show updatePos getToken where
  getToken (KW_FLOAT pos) = Just (KW_FLOAT pos)
  getToken _       = Nothing

kwString :: StateType Token
kwString = tokenPrim show updatePos getToken where
  getToken (KW_STRING pos) = Just (KW_STRING pos)
  getToken _       = Nothing

kwBool :: StateType Token
kwBool = tokenPrim show updatePos getToken where
  getToken (KW_BOOL pos) = Just (KW_BOOL pos)
  getToken _       = Nothing

kwFor :: StateType Token
kwFor = tokenPrim show updatePos getToken where
  getToken (KW_FOR pos) = Just (KW_FOR pos)
  getToken _       = Nothing

kwForeach :: StateType Token
kwForeach = tokenPrim show updatePos getToken where
  getToken (KW_FOREACH pos) = Just (KW_FOREACH pos)
  getToken _       = Nothing

kwIn :: StateType Token
kwIn = tokenPrim show updatePos getToken where
  getToken (KW_IN pos) = Just (KW_IN pos)
  getToken _       = Nothing

kwWhile :: StateType Token
kwWhile = tokenPrim show updatePos getToken where
  getToken (KW_WHILE pos) = Just (KW_WHILE pos)
  getToken _       = Nothing

kwBreak :: StateType Token
kwBreak = tokenPrim show updatePos getToken where
  getToken (KW_BREAK pos) = Just (KW_BREAK pos)
  getToken _       = Nothing

kwContinue :: StateType Token
kwContinue = tokenPrim show updatePos getToken where
  getToken (KW_CONTINUE pos) = Just (KW_CONTINUE pos)
  getToken _       = Nothing

kwRef :: StateType Token
kwRef = tokenPrim show updatePos getToken where
  getToken (KW_REF pos) = Just (KW_REF pos)
  getToken _       = Nothing

kwDeref :: StateType Token
kwDeref = tokenPrim show updatePos getToken where
  getToken (KW_DEREF pos) = Just (KW_DEREF pos)
  getToken _       = Nothing

kwFunc :: StateType Token
kwFunc = tokenPrim show updatePos getToken where
  getToken (KW_FUNC pos) = Just (KW_FUNC pos)
  getToken _       = Nothing

kwProc :: StateType Token
kwProc = tokenPrim show updatePos getToken where
  getToken (KW_FUNC pos) = Just (KW_FUNC pos)
  getToken _       = Nothing

kwEnum :: StateType Token
kwEnum = tokenPrim show updatePos getToken where
  getToken (KW_ENUM pos) = Just (KW_ENUM pos)
  getToken _       = Nothing

kwElse :: StateType Token
kwElse = tokenPrim show updatePos getToken where
  getToken (KW_ELSE pos) = Just (KW_ELSE pos)
  getToken _       = Nothing

kwStruct :: StateType Token
kwStruct = tokenPrim show updatePos getToken where
  getToken (KW_STRUCT pos) = Just (KW_STRUCT pos)
  getToken _       = Nothing

kwConst :: StateType Token
kwConst = tokenPrim show updatePos getToken where
  getToken (KW_CONST pos) = Just (KW_CONST pos)
  getToken _       = Nothing

kwImpl :: StateType Token
kwImpl = tokenPrim show updatePos getToken where
  getToken (KW_IMPL pos) = Just (KW_IMPL pos)
  getToken _       = Nothing

kwTrue :: StateType Token
kwTrue = tokenPrim show updatePos getToken where
  getToken (KW_TRUE pos) = Just (KW_TRUE pos)
  getToken _       = Nothing

kwFalse :: StateType Token
kwFalse = tokenPrim show updatePos getToken where
  getToken (KW_FALSE pos) = Just (KW_FALSE pos)
  getToken _       = Nothing

kwReturn :: StateType Token
kwReturn = tokenPrim show updatePos getToken where
  getToken (KW_RETURN pos) = Just (KW_RETURN pos)
  getToken _       = Nothing

kwImport :: StateType Token
kwImport = tokenPrim show updatePos getToken where
  getToken (KW_IMPORT pos) = Just (KW_IMPORT pos)
  getToken _       = Nothing

kwPublic :: StateType Token
kwPublic = tokenPrim show updatePos getToken where
  getToken (KW_PUBLIC pos) = Just (KW_PUBLIC pos)
  getToken _       = Nothing

kwPrivate :: StateType Token
kwPrivate = tokenPrim show updatePos getToken where
  getToken (KW_PRIVATE pos) = Just (KW_PRIVATE pos)
  getToken _       = Nothing

kwTil :: StateType Token
kwTil = tokenPrim show updatePos getToken where
  getToken (KW_TIL pos) = Just (KW_TIL pos)
  getToken _       = Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (_:_) = incSourceColumn pos 1 -- TODO does this really work ?
updatePos pos _ []    = pos
