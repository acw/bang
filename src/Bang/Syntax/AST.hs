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

instance Ord Name where
  compare (Name _ _ x _) (Name _ _ y _) = compare x y
  --
  max     n1@(Name _ _ x _) n2@(Name _ _ y _) = if x > y then n1 else n2
  min     n1@(Name _ _ x _) n2@(Name _ _ y _) = if x > y then n2 else n1
  --
  (Name _ _ x _) <  (Name _ _ y _) = x <  y
  (Name _ _ x _) <= (Name _ _ y _) = x <= y
  (Name _ _ x _) >= (Name _ _ y _) = x >= y
  (Name _ _ x _) >  (Name _ _ y _) = x >  y

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
          | TypeLambda Location Kind [Type] Type
          | TypeApp    Location Kind Type   Type
 deriving (Show)

instance Eq Type where
  (TypeUnit _ _)         == (TypeUnit _ _)         = True
  (TypePrim _ _ a)       == (TypePrim _ _ b)       = a == b
  (TypeRef  _ _ n)       == (TypeRef  _ _ m)       = n == m
  (TypeLambda _ _ at et) == (TypeLambda _ _ bt ft) = (at == bt) && (et == ft)
  (TypeApp    _ _ at bt) == (TypeApp    _ _ ct dt) = (at == ct) && (bt == dt)
  _                      == _                      = False

kind :: Type -> Kind
kind (TypeUnit   _ k)     = k
kind (TypePrim   _ k _)   = k
kind (TypeRef    _ k _)   = k
kind (TypeLambda _ k _ _) = k
kind (TypeApp    _ k _ _) = k

data Kind = Star
          | KindArrow Kind Kind
  deriving (Show, Eq)
