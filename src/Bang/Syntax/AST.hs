module Bang.Syntax.AST
 where

import Data.Text.Lazy(Text)
import Bang.Syntax.Location
import Bang.Syntax.Token

data Name = Name Text

identToName :: Located Token -> Name
identToName = undefined

data Module = Module Name [Declaration]

data Declaration = TypeDeclaration
                 | ValueDeclaration

data Expression = Expression

data Type = Type
