module Bang.Syntax.AST
 where

import Data.Text.Lazy(Text)
import Bang.Syntax.Location

data NameEnvironment = ModuleEnv | TypeEnv | VarEnv
 deriving (Eq, Ord, Show)

data Name = Name Location NameEnvironment Word Text
 deriving (Show)

data Module = Module Name [Declaration]
 deriving (Show)

data Declaration = TypeDeclaration  !Name !Type
                 | ValueDeclaration !Name !Expression
                 | PrimTypeDecl           !PrimitiveType
 deriving (Show)

data PrimitiveType = PrimType Name Text
 deriving (Show)

data Expression = ConstantExp  Location ConstantVal
                | ReferenceExp Location Name
 deriving (Show)

data ConstantVal = ConstantInt    Word Text
                 | ConstantChar        Text
                 | ConstantString      Text
                 | ConstantFloat       Text
 deriving (Show)

data Type = TypeRef Location Name
 deriving (Show)
