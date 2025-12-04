module Types where
import qualified Data.HashTable.IO as H
import Text.Parsec
import Scanner
import qualified Data.Vector as V

type SymbolType = (String, Type, Maybe Value)
type SymbolTableEntry = ([Type], Maybe Value)
type SymbolTableType = H.BasicHashTable String ([Type], Maybe Value)
type NamespaceSymbolTableType = H.BasicHashTable String ([Type], Maybe Value, AccessModifiers)
type SymbolTableStackType = [(SymbolTableType, Bool, Int)]

type LibMethodSignature = [Value] -> AlexPosn -> StateType (Maybe Value)

data InterpreterState = InterpreterState
  { parserBlock :: ParserBlock
  , programState :: ProgramState
  , symbolTableStack :: SymbolTableStackType
  , globalSymbolTable :: SymbolTableType
  , imports :: H.BasicHashTable String Bool
  , importStack :: [String]
  , nestedImportCounter :: Int
  , nextScopeId :: Int
  , namespaceStack :: [String]
  }

type StateType = ParsecT [Token] InterpreterState IO

data AccessModifiers = Static
                     | Public
                     | Private
                     deriving (Eq)

data ParserBlock = GlobalScope
                 | Method (Maybe Type) -- Target return type
                 | Loop (Maybe Type) -- Target return type
                 deriving (Eq, Show)

data ProgramState = Starting
                  | Running
                  | Skip
                  | Continue
                  | Break
                  | Return (Maybe (Type, Value)) -- Return value
                  | Finished
                  deriving (Show)

data Type = IntType
          | FloatType
          | BoolType
          | StringType
          | TemplateType (Maybe String) -- (template_symbol)
          | RefType Type
          | ConstType Type                                           -- (internal_type)
          | ArrayType Type                                           -- (internal_type)                 
          | EnumLabelType String                                     -- (enum_type_id)
          | ProcType [String] [(String, Type)] [Token]               -- (template_ids, param_types, method_body)
          | FuncType [String] [(String, Type)] Type [Token]          -- (template_ids, (param_ids, param_types), return_type, method_body)
          | StructType String [String] SymbolTableType SymbolTableType -- (struct_id, template_ids, public_data, private_data)
          | StructInstanceType String                                -- (struct_type_id)
          | ImplType SymbolTableType SymbolTableType                 -- (public_table, private_table)
          | HaskellMethod [Type] (Maybe Type) LibMethodSignature     -- (param_types, return_type, internal_method)

data Value = IntValue Int
           | FloatValue Float
           | BoolValue Bool
           | StringValue String
           | ConstValue Value
           | ArrayValue (V.Vector (Maybe Value))
           | EnumValue String
           | RefValue String Int -- (referent_id, scope_id)
           | StructValue SymbolTableType    -- (internal_symbol_table)
           deriving (Show)

instance Eq ProgramState where
    Starting == Starting = True
    Running == Running = True
    Skip == Skip = True
    Continue == Continue = True
    Break == Break = True
    Return _ == Return _ = True
    Finished == Finished = True

    _ == _ = False

instance Show Type where
    show IntType = "int"
    show FloatType = "float"
    show BoolType = "bool"
    show StringType = "string"
    show (TemplateType Nothing) = "template"
    show (TemplateType (Just name)) = "template<" ++ name ++ ">"
    show (RefType t) = "ref(" ++ show t ++ ")"
    show (ConstType t) = "const(" ++ show t ++ ")"
    show (ArrayType t) = show t ++ "[]"
    show (ProcType templates params _body) =
        "proc<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")"
    show (FuncType templates params returnT _) =
        "func<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")" ++ " -> " ++ show returnT
    show (StructType structId templates _ _) = "struct " ++ structId ++ " <" ++ show templates ++ ">"
    show (ImplType _ _) = "impl"
    show (StructInstanceType structId) = "struct " ++ structId
    show (EnumLabelType enumId) = "enum " ++ enumId
    show (HaskellMethod paramTypes _ _) =
        "internalMethod" ++ show paramTypes

showParams :: [(String, Type)] -> String
showParams = concatMap (\(_, t) -> show t ++ ", ")

instance Eq Type where
    IntType == IntType = True
    FloatType == FloatType = True
    BoolType == BoolType = True
    StringType == StringType = True
    TemplateType _ == TemplateType _ = True
    RefType t1 == RefType t2 = t1 == t2
    ConstType t1 == ConstType t2 = t1 == t2
    ArrayType t1 == ArrayType t2 = t1 == t2
    ProcType templates1 params1 _ == ProcType templates2 params2 _ =
        templates1 == templates2 && params1 == params2
    FuncType templates1 params1 r1 _ == FuncType templates2 params2 r2 _ =
        templates1 == templates2 && params1 == params2 && r1 == r2
    StructType s1 _ _ _ == StructType s2 _ _ _ = s1 == s2
    StructInstanceType s1 == StructInstanceType s2 = s1 == s2
    StructType s1 _ _ _ == StructInstanceType s2 = s1 == s2
    StructInstanceType s1 == StructType s2 _ _ _ = s1 == s2
    EnumLabelType e1 == EnumLabelType e2 = e1 == e2

    -- Implicit cast
    IntType == FloatType = True
    FloatType == IntType = True

    IntType == BoolType = True
    BoolType == IntType = True

    FloatType == BoolType = True
    BoolType == FloatType = True

    ConstType underlyingT == a = underlyingT == a
    a == ConstType underlyingT = underlyingT == a


-- Imagine a função swap<T>(T a, T b). O interpretador espera receber T. O usuário passa int. 
-- O interpretador pergunta: IntType == TemplateType? 
-- Graças a essa regra (T é igual a qualquer coisa), a resposta é SIM. O interpretador aceita a chamada sem precisar de uma lógica complexa de inferência de tipos.
    TemplateType _ == _ = True
    _ == TemplateType _  = True

    _ == _ = False
