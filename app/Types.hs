module Types where
import qualified Data.HashTable.IO as H
import Text.Parsec
import Scanner
import qualified Data.Vector as V

-- type cria apenas um sinônimo
-- data cria uma estrutura totalmente nova (parecido com um class ou struct), são os TADs

type SymbolType = (String, Type, Maybe Value)
type SymbolTableType = H.BasicHashTable String ([Type], Maybe Value)
type NamespaceSymbolTableType = H.BasicHashTable String ([Type], Maybe Value, AccessModifiers)
type SymbolTableStackType = [(SymbolTableType, Bool, Int)]

type LibMethodSignature = [Value] -> AlexPosn -> StateType (Maybe Value)

-- registro (como uma struct em C), "coração" da memória do programa em execução
data InterpreterState = InterpreterState
  { parserBlock :: ParserBlock                  -- "onde estou no código?"
  , programState :: ProgramState                -- "o que devo fazer agora? break? return?"
  , symbolTableStack :: SymbolTableStackType    -- Pilha de tabelas de símbolo. Escopo Estático e Aninhado definido aqui
  , globalSymbolTable :: SymbolTableType
  , imports :: H.BasicHashTable String Bool
  , nestedImportCounter :: Int
  , nextScopeId :: Int
  }

type StateType = ParsecT [Token] InterpreterState IO

data AccessModifiers = Static
                     | Public
                     | Private
                     deriving (Eq) -- Faz igualdade padrão (só é igual se for igual). Sem isso, precisaria do boilerplate, igual tem no fim com instance Eq Type

data ParserBlock = GlobalScope
                 | Method (Maybe Type) -- Target return type
                 | Loop (Maybe Type) -- Target return type
                 deriving (Eq, Show)

-- "sequenciadores"
data ProgramState = Starting
                  | Running
                  | Skip
                  | Continue
                  | Break
                  | Return (Maybe (Type, Value)) -- Return value
                  | Finished

-- definimos os tipos que existem em tempo de compilação
data Type = IntType
          | FloatType
          | CharType
          | BoolType
          | StringType
          | TemplateType (Maybe String) -- (template_symbol) para subprogramas genéricos
          | RefType Type
          | ConstType Type                                           -- (internal_type)
          | ArrayType Type                                           -- (internal_type)  Tipo recursivo: array de quê?               
          | EnumDeclType String SymbolTableType                      -- (enum_id, valid_labels)
          | EnumLabelType String                                     -- (enum_type_id)
          | ProcType [String] [(String, Type)] [Token]               -- (template_ids, param_types, method_body) Procedimentos (sem retorno)
          | FuncType [String] [(String, Type)] Type [Token]          -- (template_ids, (param_ids, param_types), return_type, method_body) Funções (com retorno)
          | StructType [String] SymbolTableType SymbolTableType      -- (template_ids, table_public_data, table_private_methods)
          | StructInstanceType String                                -- (struct_type_id)
          | FuncRefType [String] [Type] Type                         -- (templates_ids, param_types, return_type)
          | ProcRefType [String] [Type]                              -- (templates_ids, param_types)
          | ImplType SymbolTableType SymbolTableType SymbolTableType -- (public_table, private_table, static_table)
          | NamespaceType SymbolTableType SymbolTableType            -- (public_symbol_table, private_symbol_table)
          | HaskellMethod [Type] (Maybe Type) LibMethodSignature     -- (param_types, return_type, internal_method)

-- Aqui definimos o que existe na memória em tempo de execução.
-- Haskell é fortemente tipado. Para contornar isso e permitir que interpretador guarde qualquer coisa, criamos esse tipo "Wrapper" (tudo é um Value)
data Value = IntValue Int
           | FloatValue Float
           | CharValue Char
           | BoolValue Bool
           | StringValue String
           | ConstValue Value
           | ArrayValue (V.Vector (Maybe Value))
           | EnumValue String
           | RefValue String Int -- (referent_id, scope_id)
           | FuncRefValue String
           | ProcRefValue String
           | StructValue SymbolTableType    -- (internal_symbol_table)
           deriving (Show)

-- ensinando o operador == a comparar dois estados
instance Eq ProgramState where
    Starting == Starting = True
    Running == Running = True
    Skip == Skip = True
    Continue == Continue = True
    Break == Break = True
    Return _ == Return _ = True
    Finished == Finished = True

    _ == _ = False

-- ensinando a função show (converter para String) a imprimir nossos tipos (vital para msg de erro)
instance Show Type where
    show IntType = "int"
    show FloatType = "float"
    show CharType = "char"
    show BoolType = "bool"
    show StringType = "string"
    show (TemplateType Nothing) = "template"
    show (TemplateType (Just name)) = "template<" ++ name ++ ">"
    show (RefType t) = "ref(" ++ show t ++ ")"
    show (ConstType t) = "const(" ++ show t ++ ")"
    show (ArrayType t) = show t ++ "[]"
    show (EnumDeclType name _labelsId) = "enum " ++ name
    show (ProcType templates params _body) =
        "proc<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")"
    show (FuncType templates params returnT _) =
        "func<" ++ show templates ++ ">" ++ "(" ++ showParams params ++ ")" ++ " -> " ++ show returnT
    show (StructType templates _ _) = "structDecl" ++ show templates ++ ">"
    show (StructInstanceType structId) = "struct " ++ structId
    show (EnumLabelType enumId) = "enum " ++ enumId
    show (FuncRefType templates params returnT) =
        "funcRef<" ++ show templates ++ ">" ++
        "(" ++ show params ++ ") -> " ++ show returnT
    show (ProcRefType templates params) =
        "procRef<" ++ show templates ++ ">" ++
        "(" ++ show params ++ ")"
    show (NamespaceType _ _) = "namespace"
    show (ImplType {}) = "namespace"
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
    TemplateType _ == TemplateType _ = True
    RefType t1 == RefType t2 = t1 == t2
    ConstType t1 == ConstType t2 = t1 == t2
    ArrayType t1 == ArrayType t2 = t1 == t2
    EnumDeclType n1 _ == EnumDeclType n2 _ = n1 == n2
    ProcType templates1 params1 _ == ProcType templates2 params2 _ =
        templates1 == templates2 && params1 == params2
    FuncType templates1 params1 r1 _ == FuncType templates2 params2 r2 _ =
        templates1 == templates2 && params1 == params2 && r1 == r2
    StructType {} == StructType {} = False
    StructInstanceType s1 == StructInstanceType s2 = s1 == s2
    EnumLabelType e1 == EnumLabelType e2 = e1 == e2
    FuncRefType templates1 params1 r1 == FuncRefType templates2 params2 r2 =
        templates1 == templates2 && params1 == params2 && r1 == r2
    ProcRefType templates1 params1 == ProcRefType templates2 params2 =
        templates1 == templates2 && params1 == params2

-- Imagine a função swap<T>(T a, T b). O interpretador espera receber T. O usuário passa int. 
-- O interpretador pergunta: IntType == TemplateType? 
-- Graças a essa regra (T é igual a qualquer coisa), a resposta é SIM. O interpretador aceita a chamada sem precisar de uma lógica complexa de inferência de tipos.
    TemplateType _ == _ = True
    _ == TemplateType _  = True

    _ == _ = False
