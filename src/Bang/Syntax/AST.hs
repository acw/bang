module Bang.Syntax.AST
 where

import Data.Text.Lazy(Text, unpack)
import Bang.Syntax.Location
import Text.PrettyPrint.Annotated

data NameEnvironment = ModuleEnv | TypeEnv | VarEnv
 deriving (Eq, Ord, Show)

data Name = Name Location NameEnvironment Word Text
 deriving (Show)

ppName :: Name -> Doc a
ppName (Name _ _ _ t) = text' t

data Module = Module Name [Declaration]
 deriving (Show)

ppModule :: Module -> Doc a
ppModule (Module name decls) =
  vcat ([text "module" <> space <> ppName name, text ""] ++
        map ppDeclaration decls)

data Declaration = TypeDeclaration     Name Type
                 | ValueDeclaration    Name Expression
                 | PrimTypeDeclaration Name Text
 deriving (Show)

ppDeclaration :: Declaration -> Doc a
ppDeclaration d =
  case d of
    TypeDeclaration n t ->
      ppName n <> space <> text "::" <> space <> ppType t
    ValueDeclaration n e ->
      ppName n <> space <> text "=" <> space <> ppExpression e
    PrimTypeDeclaration n t ->
      text "primitive" <> space <> text "type" <> space <>
      ppName n <> space <> text "=" <> space <> text' t

data Expression = ConstantExp  Location ConstantValue
                | ReferenceExp Location Name
 deriving (Show)

ppExpression :: Expression -> Doc a
ppExpression e =
  case e of
    ConstantExp  _ v -> ppConstantValue v
    ReferenceExp _ n -> ppName n

data ConstantValue = ConstantInt    Word Text
                   | ConstantChar        Text
                   | ConstantString      Text
                   | ConstantFloat       Text
 deriving (Show)

ppConstantValue :: ConstantValue -> Doc a
ppConstantValue cv =
  case cv of
    ConstantInt    2  t -> text "0b" <> text' t
    ConstantInt    8  t -> text "0o" <> text' t
    ConstantInt    10 t ->              text' t
    ConstantInt    16 t -> text "0x" <> text' t
    ConstantChar      c -> text' c
    ConstantString    s -> text' s
    ConstantFloat     f -> text' f

data Type = TypeRef Location Name
 deriving (Show)

ppType :: Type -> Doc a
ppType t =
  case t of
    TypeRef _ n -> ppName n

text' :: Text -> Doc a
text' = text . unpack
