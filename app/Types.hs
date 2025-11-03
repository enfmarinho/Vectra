module Types where
import qualified Data.HashTable.IO as H
import Text.Parsec
import Scanner
import Data.Array

type SymbolType = (String, Type)
type SymbolTableType = H.BasicHashTable String [Type]
type SymbolTableStackType = [(SymbolTableType, Bool)]

type MemoryType = (String, Value)
type MemoryTableType = H.BasicHashTable String Value
type MemoryTableStackType = [MemoryTableType]

data ParserState = ParserState
  { programState :: ProgramState
  , symbolTableStack :: SymbolTableStackType
  , memoryTableStack :: MemoryTableStackType
  , globalSymbolTable :: SymbolTableType
  , globalMemoryTable :: MemoryTableType
  }

type StateType = ParsecT [Token] ParserState IO

data ProgramState = Starting
                  | Running
                  | Finished
                  deriving (Eq)

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
          deriving (Show)

data Value = IntValue Int                     
           | FloatValue Float                 
           | CharValue Char                   
           | BoolValue Bool                   
           | StringValue String               
           | ConstValue Value                 
           | ArrayValue (Array Int Value)
           | EnumValue String
           | FuncRefValue String
           | ProcRefValue String
           | StructValue SymbolTableType MemoryTableType    -- (internal_symbol_table, internal_memory)
           | NamespaceValue SymbolTableType MemoryTableType -- (internal_symbol_table, internal_memory)

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
