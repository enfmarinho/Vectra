module TerminalTokens where

import Scanner
import Text.Parsec
import SymbolTable

id :: StateType Token
id = tokenPrim show update_pos get_token where
  get_token (ID pos s) = Just (ID pos s)
  get_token _      = Nothing

intLiteral :: StateType Token 
intLiteral = tokenPrim show update_pos get_token where
  get_token (INT_LITERAL pos x) = Just (INT_LITERAL pos x)
  get_token _       = Nothing

floatLiteral :: StateType Token
floatLiteral = tokenPrim show update_pos get_token where
  get_token (FLOAT_LITERAL pos x) = Just (FLOAT_LITERAL pos x)
  get_token _       = Nothing

stringLiteral :: StateType Token
stringLiteral = tokenPrim show update_pos get_token where
  get_token (STRING_LITERAL pos s) = Just (STRING_LITERAL pos s)
  get_token _       = Nothing

kwSemicolumn :: StateType Token
kwSemicolumn = tokenPrim show update_pos get_token where
  get_token (KW_SEMICOLUMN pos) = Just (KW_SEMICOLUMN pos)
  get_token _       = Nothing

kwColumn :: StateType Token
kwColumn = tokenPrim show update_pos get_token where
  get_token (KW_COLUMN pos) = Just (KW_COLUMN pos)
  get_token _       = Nothing

kwComma :: StateType Token
kwComma = tokenPrim show update_pos get_token where
  get_token (KW_COMMA pos) = Just (KW_COMMA pos)
  get_token _       = Nothing

kwAssingment :: StateType Token
kwAssingment = tokenPrim show update_pos get_token where
  get_token (KW_ASSIGNMENT pos) = Just (KW_ASSIGNMENT pos)
  get_token _       = Nothing

opNot :: StateType Token
opNot = tokenPrim show update_pos get_token where
  get_token (OP_NOT pos) = Just (OP_NOT pos)
  get_token _       = Nothing

opAnd :: StateType Token
opAnd = tokenPrim show update_pos get_token where
  get_token (OP_AND pos) = Just (OP_AND pos)
  get_token _       = Nothing

opOr :: StateType Token
opOr = tokenPrim show update_pos get_token where
  get_token (OP_OR pos) = Just (OP_OR pos)
  get_token _       = Nothing

opAdd :: StateType Token
opAdd = tokenPrim show update_pos get_token where
  get_token (OP_ADD pos) = Just (OP_ADD pos)
  get_token _       = Nothing

opSub:: StateType Token
opSub = tokenPrim show update_pos get_token where
  get_token (OP_SUB pos) = Just (OP_SUB pos)
  get_token _       = Nothing

opMult :: StateType Token
opMult = tokenPrim show update_pos get_token where
  get_token (OP_MULT pos) = Just (OP_MULT pos)
  get_token _       = Nothing

opDiv :: StateType Token
opDiv = tokenPrim show update_pos get_token where
  get_token (OP_DIV pos) = Just (OP_DIV pos)
  get_token _       = Nothing

-- TODO: maybe not a string associated to it but a specific TOKEN for every possible comparison?
opCompare :: StateType Token
opCompare = tokenPrim show update_pos get_token where
  get_token (OP_COMPARE pos s) = Just (OP_COMPARE pos s)
  get_token _       = Nothing

openParen :: StateType Token
openParen = tokenPrim show update_pos get_token where
  get_token (OPEN_PAREN pos) = Just (OPEN_PAREN pos)
  get_token _       = Nothing

closeParen :: StateType Token
closeParen = tokenPrim show update_pos get_token where
  get_token (CLOSE_PAREN pos) = Just (CLOSE_PAREN pos)
  get_token _       = Nothing

openBracket :: StateType Token
openBracket = tokenPrim show update_pos get_token where
  get_token (OPEN_BRACKET pos) = Just (OPEN_BRACKET pos)
  get_token _       = Nothing

closeBracket :: StateType Token
closeBracket = tokenPrim show update_pos get_token where
  get_token (CLOSE_BRACKET pos) = Just (CLOSE_BRACKET pos)
  get_token _       = Nothing

indent :: StateType Token
indent = tokenPrim show update_pos get_token where
  get_token INDENT = Just INDENT
  get_token _       = Nothing

unindent :: StateType Token
unindent = tokenPrim show update_pos get_token where
  get_token UNINDENT = Just UNINDENT
  get_token _       = Nothing

newLine :: StateType Token
newLine = tokenPrim show update_pos get_token where
  get_token (NEWLINE pos) = Just (NEWLINE pos)
  get_token _       = Nothing

kwIf :: StateType Token
kwIf = tokenPrim show update_pos get_token where
  get_token (KW_IF pos) = Just (KW_IF pos)
  get_token _       = Nothing

kwInt :: StateType Token
kwInt = tokenPrim show update_pos get_token where
  get_token (KW_INT pos) = Just (KW_INT pos)
  get_token _       = Nothing

kwFloat :: StateType Token
kwFloat = tokenPrim show update_pos get_token where
  get_token (KW_FLOAT pos) = Just (KW_FLOAT pos)
  get_token _       = Nothing

kwString :: StateType Token
kwString = tokenPrim show update_pos get_token where
  get_token (KW_STRING pos) = Just (KW_STRING pos)
  get_token _       = Nothing

kwBool :: StateType Token
kwBool = tokenPrim show update_pos get_token where
  get_token (KW_BOOL pos) = Just (KW_BOOL pos)
  get_token _       = Nothing

kwFor :: StateType Token
kwFor = tokenPrim show update_pos get_token where
  get_token (KW_FOR pos) = Just (KW_FOR pos)
  get_token _       = Nothing

kwWhile :: StateType Token
kwWhile = tokenPrim show update_pos get_token where
  get_token (KW_WHILE pos) = Just (KW_WHILE pos)
  get_token _       = Nothing

kwRef :: StateType Token
kwRef = tokenPrim show update_pos get_token where
  get_token (KW_REF pos) = Just (KW_REF pos)
  get_token _       = Nothing

kwDeref :: StateType Token
kwDeref = tokenPrim show update_pos get_token where
  get_token (KW_DEREF pos) = Just (KW_DEREF pos)
  get_token _       = Nothing

kwFunc :: StateType Token
kwFunc = tokenPrim show update_pos get_token where
  get_token (KW_FUNC pos) = Just (KW_FUNC pos)
  get_token _       = Nothing

kwEnum :: StateType Token
kwEnum = tokenPrim show update_pos get_token where
  get_token (KW_ENUM pos) = Just (KW_ENUM pos)
  get_token _       = Nothing

kwElse :: StateType Token
kwElse = tokenPrim show update_pos get_token where
  get_token (KW_ELSE pos) = Just (KW_ELSE pos)
  get_token _       = Nothing

kwBlock :: StateType Token
kwBlock = tokenPrim show update_pos get_token where
  get_token (KW_BLOCK pos) = Just (KW_BLOCK pos)
  get_token _       = Nothing

kwConst :: StateType Token
kwConst = tokenPrim show update_pos get_token where
  get_token (KW_CONST pos) = Just (KW_CONST pos)
  get_token _       = Nothing

kwTrue :: StateType Token
kwTrue = tokenPrim show update_pos get_token where
  get_token (KW_TRUE pos) = Just (KW_TRUE pos)
  get_token _       = Nothing

kwFalse :: StateType Token
kwFalse = tokenPrim show update_pos get_token where
  get_token (KW_FALSE pos) = Just (KW_FALSE pos)
  get_token _       = Nothing

kwReturn :: StateType Token
kwReturn = tokenPrim show update_pos get_token where
  get_token (KW_RETURN pos) = Just (KW_RETURN pos)
  get_token _       = Nothing

kwImport :: StateType Token
kwImport = tokenPrim show update_pos get_token where
  get_token (KW_IMPORT pos) = Just (KW_IMPORT pos)
  get_token _       = Nothing

kwPublic :: StateType Token
kwPublic = tokenPrim show update_pos get_token where
  get_token (KW_PUBLIC pos) = Just (KW_PUBLIC pos)
  get_token _       = Nothing

kwPrivate :: StateType Token
kwPrivate = tokenPrim show update_pos get_token where
  get_token (KW_PRIVATE pos) = Just (KW_PRIVATE pos)
  get_token _       = Nothing

kwTil :: StateType Token
kwTil = tokenPrim show update_pos get_token where
  get_token (KW_TIL pos) = Just (KW_TIL pos)
  get_token _       = Nothing


-- TODO my linter complains that this is in snake case, should be in camelCase to follow standard
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (_:_) = incSourceColumn pos 1 -- TODO does this really work ?
update_pos pos _ []       = pos
