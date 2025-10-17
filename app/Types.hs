module Types where

data Type = IntType Int |
            FloatType Float | 
            CharType Char | 
            BoolType Bool |
            StringType String |
            FuncType String Type [(String, Type)] |
            EnumType String [(String, Int)] | 
            BlockType String [String] [String] |
            ArrayType String Type Int
