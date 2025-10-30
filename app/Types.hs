module Types where
import qualified Data.HashTable.IO as H
import Text.Parsec
import Scanner
import Control.Monad.State.Lazy

type SymbolType = (String, Type)
type SymbolTableType = H.BasicHashTable String Type
type SymbolTableStackType = [SymbolTableType]
type SymbolTableStackState = StateT SymbolTableStackType IO

type MemoryType = (String, Value)
type MemoryTableType = H.BasicHashTable String Value
type MemoryTableStackType = [MemoryTableType]
type MemoryTableStackState = StateT MemoryTableStackType IO

type StateType = ParsecT [Token] SymbolTableStackType IO
-- type StateType = ParsecT [Token] (SymbolTableStackType, MemoryTableStackType, Bool) IO

data Type = IntType                               
          | FloatType                             
          | CharType                              
          | BoolType                              
          | StringType                            
          | TemplateType                          
          | ConstType Type                                      -- (internal_type)
          | ArrayType Int Type                                  -- (size, type)                 
          | EnumType [String]                                   -- (valid_labels)
          | ProcType [String] [Type] [Token]                    -- (template_ids, param_types, method_body)
          | FuncType [String] [Type] Type [Token]               -- (template_ids, param_types, return_type, method_body)
          | BlockType [String] SymbolTableType SymbolTableType  -- (template_ids, table_for_methods, table_for_data)
          | BlockInstanceType String                            -- (block_type_id)
          | EnumInstanceType String                             -- (enum_type_id)

data Value = IntValue Int                     
           | FloatValue Float                 
           | CharValue Char                   
           | BoolValue Bool                   
           | StringValue String               
           | ConstValue Value                 
           | ArrayValue [Value]               
           | EnumValue String
           | BlockValue SymbolTableType MemoryTableType -- (internal_symbol_table, internal_memory)
