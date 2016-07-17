{-# LANGUAGE TemplateHaskell #-}
module Bang.AST.Declaration
       ( Declaration(..)
       , ppDeclaration
       , declName
       -- * Type Declarations
       , TypeDeclaration
       , ppTypeDeclaration
       , mkTypeDecl
       , tdName, tdLocation, tdType
       -- * Value Declarations
       , ValueDeclaration
       , ppValueDeclaration
       , mkValueDecl
       , vdName, vdLocation
       , vdFreeTypeVariables, vdFreeValueVariables
       , vdDeclaredType, vdValue
       )
 where

import Bang.AST.Expression(Expression, ppExpression)
import Bang.AST.Name(Name, ppName)
import Bang.AST.Type(Type(TypePrim), ppType)
import Bang.Syntax.Location(Location)
import Control.Lens(Lens', view, set, lens)
import Control.Lens(makeLenses)
import Text.PrettyPrint.Annotated(Doc, text, (<+>), ($+$), (<>), empty)
import Text.PrettyPrint.Annotated(braces, punctuate, comma, hsep, space)

data TypeDeclaration = TypeDeclaration
     { _tdName     :: Name
     , _tdLocation :: Location
     , _tdType     :: Type
     }
 deriving (Show)

class MkTypeDecl a where
  mkTypeDecl :: Name -> Location -> Type -> a

ppTypeDeclaration :: TypeDeclaration -> Doc a
ppTypeDeclaration td = prefix <> text "type" <+> ppName (_tdName td) <+>
                       text "=" <+> ppType (_tdType td)
 where
  prefix | TypePrim _ <- _tdType td = text "primitive" <> space
         | otherwise                = empty

instance MkTypeDecl TypeDeclaration where
  mkTypeDecl = TypeDeclaration

instance MkTypeDecl Declaration where
  mkTypeDecl n l t = DeclType (TypeDeclaration n l t)

-- -----------------------------------------------------------------------------

data ValueDeclaration = ValueDeclaration
     { _vdName               :: Name
     , _vdLocation           :: Location
     , _vdFreeTypeVariables  :: [Name]
     , _vdFreeValueVariables :: [Name]
     , _vdDeclaredType       :: Maybe Type
     , _vdValue              :: Expression
     }
 deriving (Show)

class MkValueDecl a where
  mkValueDecl :: Name -> Location -> Maybe Type -> Expression -> a

ppValueDeclaration :: ValueDeclaration -> Doc a
ppValueDeclaration vd = frees $+$ typedecl $+$ valuedecl
 where
  frees =
    text "free type variables: " <+>
    braces (hsep (punctuate comma (map ppName (_vdFreeTypeVariables vd)))) $+$
    text "free value variables: " <+>
    braces (hsep (punctuate comma (map ppName (_vdFreeValueVariables vd))))
  typedecl
    | Just dt <- _vdDeclaredType vd =
        ppTypeDeclaration (TypeDeclaration (_vdName vd) (_vdLocation vd) dt)
    | otherwise = empty
  valuedecl = ppName (_vdName vd) <+> text "=" <+> ppExpression (_vdValue vd)

instance MkValueDecl ValueDeclaration where
  mkValueDecl n l mt e = ValueDeclaration n l [] [] mt e

instance MkValueDecl Declaration where
  mkValueDecl n l mt e = DeclVal (ValueDeclaration n l [] [] mt e)

-- -----------------------------------------------------------------------------

data Declaration = DeclType TypeDeclaration
                 | DeclVal  ValueDeclaration
 deriving (Show)

ppDeclaration :: Declaration -> Doc a
ppDeclaration (DeclType d) = ppTypeDeclaration d
ppDeclaration (DeclVal  d) = ppValueDeclaration d

makeLenses ''TypeDeclaration
makeLenses ''ValueDeclaration

declName :: Lens' Declaration Name
declName = lens getter setter
 where
  getter (DeclType d) = view tdName d
  getter (DeclVal  d) = view vdName d
  setter (DeclType d) x = DeclType (set tdName x d)
  setter (DeclVal  d) x = DeclVal  (set vdName x d)

