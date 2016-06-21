module Bang.Syntax.AST
 where

import Data.Text.Lazy(Text)
import Bang.Syntax.Location(Location)

data NameEnvironment = ModuleEnv | TypeEnv | VarEnv
 deriving (Eq, Ord, Show)

data Name = Name Location NameEnvironment Word Text
 deriving (Show)

instance Eq Name where
  (Name _ _ x _) == (Name _ _ y _) = x == y
  (Name _ _ x _) /= (Name _ _ y _) = x /= y

data Module = Module Name [Declaration]
 deriving (Show)

data Declaration = TypeDeclaration     Name Type
                 | ValueDeclaration    Name Expression
                 | PrimTypeDeclaration Name Text
 deriving (Show)

data Expression = ConstantExp  Location ConstantValue
                | ReferenceExp Location Name
                | LambdaExp    Location [Name] Expression
 deriving (Show)

data ConstantValue = ConstantInt    Word Text
                   | ConstantChar        Text
                   | ConstantString      Text
                   | ConstantFloat       Text
 deriving (Show)

data Type = TypeUnit   Location Kind
          | TypePrim   Location Kind Text
          | TypeRef    Location Kind Name
          | TypeLambda Location Kind Type   Type
          | TypeApp    Location Kind Type   Type
          | TypeForAll               [Name] Type
 deriving (Show)

kind :: Type -> Kind
kind (TypeUnit   _ k)     = k
kind (TypePrim   _ k _)   = k
kind (TypeRef    _ k _)   = k
kind (TypeLambda _ k _ _) = k
kind (TypeApp    _ k _ _) = k
kind (TypeForAll     _ t) = kind t

data Kind = Star
          | KindArrow Kind Kind
  deriving (Show)
