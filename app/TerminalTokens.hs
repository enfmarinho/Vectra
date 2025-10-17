module TerminalTokens where

import Scanner
import Text.Parsec
import SymbolTable

-- TODO make all signatures just like the id one

id :: StateType Token
id = tokenPrim show update_pos get_token where
  get_token (ID pos s) = Just (ID pos s)
  get_token _      = Nothing

intLiteral :: StateType Token 
intLiteral = tokenPrim show update_pos get_token where
  get_token (INT_LITERAL pos x) = Just (INT_LITERAL pos x)
  get_token _       = Nothing

floatLiteral :: Parsec [Token] st Token
floatLiteral = tokenPrim show update_pos get_token where
  get_token (FLOAT_LITERAL pos x) = Just (FLOAT_LITERAL pos x)
  get_token _       = Nothing

stringLiteral :: Parsec [Token] st Token
stringLiteral = tokenPrim show update_pos get_token where
  get_token (STRING_LITERAL pos s) = Just (STRING_LITERAL pos s)
  get_token _       = Nothing

kwSemicolumn :: Parsec [Token] st Token
kwSemicolumn = tokenPrim show update_pos get_token where
  get_token (KW_SEMICOLUMN pos) = Just (KW_SEMICOLUMN pos)
  get_token _       = Nothing

kwColumn :: Parsec [Token] st Token
kwColumn = tokenPrim show update_pos get_token where
  get_token (KW_COLUMN pos) = Just (KW_COLUMN pos)
  get_token _       = Nothing

kwComma :: Parsec [Token] st Token
kwComma = tokenPrim show update_pos get_token where
  get_token (KW_COMMA pos) = Just (KW_COMMA pos)
  get_token _       = Nothing

kwAssingment :: StateType Token
kwAssingment = tokenPrim show update_pos get_token where
  get_token (KW_ASSIGNMENT pos) = Just (KW_ASSIGNMENT pos)
  get_token _       = Nothing

opNot :: Parsec [Token] st Token
opNot = tokenPrim show update_pos get_token where
  get_token (OP_NOT pos) = Just (OP_NOT pos)
  get_token _       = Nothing

opAnd :: Parsec [Token] st Token
opAnd = tokenPrim show update_pos get_token where
  get_token (OP_AND pos) = Just (OP_AND pos)
  get_token _       = Nothing

opOr :: Parsec [Token] st Token
opOr = tokenPrim show update_pos get_token where
  get_token (OP_OR pos) = Just (OP_OR pos)
  get_token _       = Nothing

opAdd :: Parsec [Token] st Token
opAdd = tokenPrim show update_pos get_token where
  get_token (OP_ADD pos) = Just (OP_ADD pos)
  get_token _       = Nothing

opSub :: Parsec [Token] st Token
opSub = tokenPrim show update_pos get_token where
  get_token (OP_SUB pos) = Just (OP_SUB pos)
  get_token _       = Nothing

opMult :: Parsec [Token] st Token
opMult = tokenPrim show update_pos get_token where
  get_token (OP_MULT pos) = Just (OP_MULT pos)
  get_token _       = Nothing

opDiv :: Parsec [Token] st Token
opDiv = tokenPrim show update_pos get_token where
  get_token (OP_DIV pos) = Just (OP_DIV pos)
  get_token _       = Nothing

-- TODO: maybe not a string associated to it but an OP for every possible comparison?
opCompare :: Parsec [Token] st Token
opCompare = tokenPrim show update_pos get_token where
  get_token (OP_COMPARE pos s) = Just (OP_COMPARE pos s)
  get_token _       = Nothing

openParen :: Parsec [Token] st Token
openParen = tokenPrim show update_pos get_token where
  get_token (OPEN_PAREN pos) = Just (OPEN_PAREN pos)
  get_token _       = Nothing

closeParen :: Parsec [Token] st Token
closeParen = tokenPrim show update_pos get_token where
  get_token (CLOSE_PAREN pos) = Just (CLOSE_PAREN pos)
  get_token _       = Nothing

openBracket :: Parsec [Token] st Token
openBracket = tokenPrim show update_pos get_token where
  get_token (OPEN_BRACKET pos) = Just (OPEN_BRACKET pos)
  get_token _       = Nothing

closeBracket :: Parsec [Token] st Token
closeBracket = tokenPrim show update_pos get_token where
  get_token (CLOSE_BRACKET pos) = Just (CLOSE_BRACKET pos)
  get_token _       = Nothing

tab :: Parsec [Token] st Token
tab = tokenPrim show update_pos get_token where
  get_token (TAB pos) = Just (TAB pos)
  get_token _       = Nothing

kwIf :: Parsec [Token] st Token
kwIf = tokenPrim show update_pos get_token where
  get_token (KW_IF pos) = Just (KW_IF pos)
  get_token _       = Nothing

kwInt :: Parsec [Token] st Token
kwInt = tokenPrim show update_pos get_token where
  get_token (KW_INT pos) = Just (KW_INT pos)
  get_token _       = Nothing

kwFloat :: Parsec [Token] st Token
kwFloat = tokenPrim show update_pos get_token where
  get_token (KW_FLOAT pos) = Just (KW_FLOAT pos)
  get_token _       = Nothing

kwString :: Parsec [Token] st Token
kwString = tokenPrim show update_pos get_token where
  get_token (KW_STRING pos) = Just (KW_STRING pos)
  get_token _       = Nothing

kwBool :: Parsec [Token] st Token
kwBool = tokenPrim show update_pos get_token where
  get_token (KW_BOOL pos) = Just (KW_BOOL pos)
  get_token _       = Nothing

kwFor :: Parsec [Token] st Token
kwFor = tokenPrim show update_pos get_token where
  get_token (KW_FOR pos) = Just (KW_FOR pos)
  get_token _       = Nothing

kwWhile :: Parsec [Token] st Token
kwWhile = tokenPrim show update_pos get_token where
  get_token (KW_WHILE pos) = Just (KW_WHILE pos)
  get_token _       = Nothing

kwRef :: Parsec [Token] st Token
kwRef = tokenPrim show update_pos get_token where
  get_token (KW_REF pos) = Just (KW_REF pos)
  get_token _       = Nothing

kwDeref :: Parsec [Token] st Token
kwDeref = tokenPrim show update_pos get_token where
  get_token (KW_DEREF pos) = Just (KW_DEREF pos)
  get_token _       = Nothing

kwFunc :: Parsec [Token] st Token
kwFunc = tokenPrim show update_pos get_token where
  get_token (KW_FUNC pos) = Just (KW_FUNC pos)
  get_token _       = Nothing

kwEnum :: Parsec [Token] st Token
kwEnum = tokenPrim show update_pos get_token where
  get_token (KW_ENUM pos) = Just (KW_ENUM pos)
  get_token _       = Nothing

kwElse :: Parsec [Token] st Token
kwElse = tokenPrim show update_pos get_token where
  get_token (KW_ELSE pos) = Just (KW_ELSE pos)
  get_token _       = Nothing

kwBlock :: Parsec [Token] st Token
kwBlock = tokenPrim show update_pos get_token where
  get_token (KW_BLOCK pos) = Just (KW_BLOCK pos)
  get_token _       = Nothing

kwConst :: Parsec [Token] st Token
kwConst = tokenPrim show update_pos get_token where
  get_token (KW_CONST pos) = Just (KW_CONST pos)
  get_token _       = Nothing

kwTrue :: Parsec [Token] st Token
kwTrue = tokenPrim show update_pos get_token where
  get_token (KW_TRUE pos) = Just (KW_TRUE pos)
  get_token _       = Nothing

kwFalse :: Parsec [Token] st Token
kwFalse = tokenPrim show update_pos get_token where
  get_token (KW_FALSE pos) = Just (KW_FALSE pos)
  get_token _       = Nothing

kwReturn :: Parsec [Token] st Token
kwReturn = tokenPrim show update_pos get_token where
  get_token (KW_RETURN pos) = Just (KW_RETURN pos)
  get_token _       = Nothing

kwImport :: Parsec [Token] st Token
kwImport = tokenPrim show update_pos get_token where
  get_token (KW_IMPORT pos) = Just (KW_IMPORT pos)
  get_token _       = Nothing

kwPublic :: Parsec [Token] st Token
kwPublic = tokenPrim show update_pos get_token where
  get_token (KW_PUBLIC pos) = Just (KW_PUBLIC pos)
  get_token _       = Nothing

kwPrivate :: Parsec [Token] st Token
kwPrivate = tokenPrim show update_pos get_token where
  get_token (KW_PRIVATE pos) = Just (KW_PRIVATE pos)
  get_token _       = Nothing

kwTil :: Parsec [Token] st Token
kwTil = tokenPrim show update_pos get_token where
  get_token (KW_TIL pos) = Just (KW_TIL pos)
  get_token _       = Nothing

-- TODO bother yourself looking into this
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos
