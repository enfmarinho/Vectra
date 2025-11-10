module Types where
import qualified Data.HashTable.IO as H
import Text.Parsec
import Scanner
import Data.Array

type SymbolType = (String, Type)
type SymbolTableType = H.BasicHashTable String [Type]
type SymbolTableStackType = [(SymbolTableType, Bool)]

type MemoryType = (String, Value)
type MemoryTableType = H.BasicHashTable String (Maybe Value)
type MemoryTableStackType = [MemoryTableType]

type LibMethodSignature = [Value] -> AlexPosn -> StateType (Maybe Value)

data InterpreterState = InterpreterState
  { parserBlock :: ParserBlock
  , programState :: ProgramState
  , symbolTableStack :: SymbolTableStackType
  , memoryTableStack :: MemoryTableStackType
  , globalSymbolTable :: SymbolTableType
  , globalMemoryTable :: MemoryTableType
  }

type StateType = ParsecT [Token] InterpreterState IO

data ParserBlock = GlobalScope
                 | Method (Maybe Type) -- Target return type
                 | Loop
                 | Conditional
                 deriving (Eq, Show)

data ProgramState = Starting
                  | Running
                  | Skip
                  | Continue
                  | Break
                  | Return (Maybe Value) -- Return value
                  | Finished

data Type = IntType
          | FloatType
          | CharType
          | BoolType
          | StringType
          | TemplateType
          | RefType Type
          | ConstType Type                                  -- (internal_type)
          | ArrayType Int Type                              -- (size, type)                 
          | EnumType [String]                               -- (valid_labels)
          | ProcType [String] [(String, Type)] [Token]      -- (template_ids, param_types, method_body)
          | FuncType [String] [(String, Type)] Type [Token] -- (template_ids, (param_ids, param_types), return_type, method_body)
          | StructType [String] SymbolTableType             -- (template_ids, table_for_data)
          | StructInstanceType String                       -- (struct_type_id)
          | EnumInstanceType String                         -- (enum_type_id)
          | FuncRefType [String] [Type] Type                -- (templates_ids, param_types, return_type)
          | ProcRefType [String] [Type]                     -- (templates_ids, param_types)
          | NamespaceType SymbolTableType                   -- (symbol_table)
          | ImplNamespaceType SymbolTableType               -- (symbol_table)
          | HaskellMethod [Type] (Maybe Type) LibMethodSignature         -- (param_types, return_type, internal_method)

data Value = IntValue Int
           | FloatValue Float
           | CharValue Char
           | BoolValue Bool
           | StringValue String
           | ConstValue Value
           | ArrayValue (Array Int Value)
           | EnumValue String
           | RefValue Int String
           | FuncRefValue String
           | ProcRefValue String
           | StructValue SymbolTableType MemoryTableType    -- (internal_symbol_table, internal_memory)
           | NamespaceValue SymbolTableType MemoryTableType -- (internal_symbol_table, internal_memory)

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
    show CharType = "char"
    show BoolType = "bool"
    show StringType = "string"
    show TemplateType = "template"
    show (RefType t) = "ref(" ++ show t ++ ")"
    show (ConstType t) = "const(" ++ show t ++ ")"
    show (ArrayType size t) = show t ++ "[" ++ show size ++ "]"
    show (EnumType _labelsId) = "enumDecl"
    show (ProcType templates params _body) =
        "proc<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")"
    show (FuncType templates params returnT _) =
        "func<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")" ++ " -> " ++ show returnT
    show (StructType templates _table) = "structDecl" ++ show templates ++ ">"
    show (StructInstanceType structId) = "struct " ++ structId
    show (EnumInstanceType enumId) = "enum " ++ enumId
    show (FuncRefType templates params returnT) =
        "funcRef<" ++ show templates ++ ">" ++
        "(" ++ show params ++ ") -> " ++ show returnT
    show (ProcRefType templates params) =
        "procRef<" ++ show templates ++ ">" ++
        "(" ++ show params ++ ")"
    show (NamespaceType _) = "namespace"
    show (ImplNamespaceType _) = "impl"
    show (HaskellMethod paramTypes _ _) =
        "internalMethod" ++ show paramTypes

showParams :: [(String, Type)] -> String
showParams = concatMap (\(_, t) -> show t ++ ", ")

instance Eq Type where
    IntType == IntType = True
    FloatType == FloatType = True
    CharType == CharType = True
    BoolType == BoolType = True
    StringType == StringType = True
    TemplateType == TemplateType = True
    RefType t1 == RefType t2 = t1 == t2
    ConstType t1 == ConstType t2 = t1 == t2
    ArrayType _ t1 == ArrayType _ t2 = t1 == t2
    EnumType _ == EnumType _ = False
    ProcType templates1 params1 _ == ProcType templates2 params2 _ =
        templates1 == templates2 && params1 == params2
    FuncType templates1 params1 r1 _ == FuncType templates2 params2 r2 _ =
        templates1 == templates2 && params1 == params2 && r1 == r2
    StructType {} == StructType {} = False
    StructInstanceType s1 == StructInstanceType s2 = s1 == s2
    EnumInstanceType e1 == EnumInstanceType e2 = e1 == e2
    FuncRefType templates1 params1 r1 == FuncRefType templates2 params2 r2 =
        templates1 == templates2 && params1 == params2 && r1 == r2
    ProcRefType templates1 params1 == ProcRefType templates2 params2 =
        templates1 == templates2 && params1 == params2

    _ == _ = False
