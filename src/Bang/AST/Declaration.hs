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
       , vdDeclaredType, vdValue
       )
 where

import Bang.AST.Expression(Expression, ppExpression)
import Bang.AST.Name(Name, ppName)
import Bang.AST.Type(Type(TypePrim), ppType)
import Bang.Syntax.Location(Location)
import Bang.Utils.FreeVars(CanHaveFreeVars(..))
import Control.Lens(Lens', view, set, lens)
import Control.Lens(makeLenses)
import Data.Set(delete, union)
import Text.PrettyPrint.Annotated(Doc, text, (<+>), ($+$), (<>), empty, space)

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

instance CanHaveFreeVars TypeDeclaration where
  freeVariables td = delete (_tdName td) (freeVariables (_tdType td))

-- -----------------------------------------------------------------------------

data ValueDeclaration = ValueDeclaration
     { _vdName               :: Name
     , _vdLocation           :: Location
     , _vdDeclaredType       :: Maybe Type
     , _vdValue              :: Expression
     }
 deriving (Show)

class MkValueDecl a where
  mkValueDecl :: Name -> Location -> Maybe Type -> Expression -> a

ppValueDeclaration :: ValueDeclaration -> Doc a
ppValueDeclaration vd = typedecl $+$ valuedecl
 where
  typedecl
    | Just t <- _vdDeclaredType vd =
        ppName (_vdName vd) <+> text "::" <+> ppType t
    | otherwise = empty
  valuedecl = ppName (_vdName vd) <+> text "=" <+> ppExpression (_vdValue vd)

instance MkValueDecl ValueDeclaration where
  mkValueDecl n l mt e = ValueDeclaration n l mt e

instance MkValueDecl Declaration where
  mkValueDecl n l mt e = DeclVal (ValueDeclaration n l mt e)

instance CanHaveFreeVars ValueDeclaration where
  freeVariables vd = delete (_vdName vd) (union valTypes typeTypes)
   where
    valTypes  = freeVariables (_vdValue vd)
    typeTypes = freeVariables (_vdDeclaredType vd)

-- -----------------------------------------------------------------------------

data Declaration = DeclType TypeDeclaration
                 | DeclVal  ValueDeclaration
 deriving (Show)

ppDeclaration :: Declaration -> Doc a
ppDeclaration (DeclType d) = ppTypeDeclaration d
ppDeclaration (DeclVal  d) = ppValueDeclaration d

instance CanHaveFreeVars Declaration where
  freeVariables (DeclType td) = freeVariables td
  freeVariables (DeclVal  vd) = freeVariables vd

makeLenses ''TypeDeclaration
makeLenses ''ValueDeclaration

declName :: Lens' Declaration Name
declName = lens getter setter
 where
  getter (DeclType d) = view tdName d
  getter (DeclVal  d) = view vdName d
  setter (DeclType d) x = DeclType (set tdName x d)
  setter (DeclVal  d) x = DeclVal  (set vdName x d)

