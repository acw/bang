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
       -- * Declarations of primitive types
       , PrimitiveDeclaration
       , ppPrimitiveDeclaration
       , mkPrimDecl
       , pdName, pdLocation, pdLLVMType
       )
 where

import Bang.AST.Expression(Expression, ppExpression)
import Bang.AST.Name(Name, ppName)
import Bang.AST.Type(Type, ppType)
import Bang.Syntax.Location(Location)
import Bang.Utils.Pretty(text')
import Data.Text.Lazy(Text)
import Control.Lens(Lens', view, set, lens)
import Control.Lens(makeLenses)
import Text.PrettyPrint.Annotated(Doc, text, (<+>), ($+$), empty)

data TypeDeclaration = TypeDeclaration
     { _tdName     :: Name
     , _tdLocation :: Location
     , _tdType     :: Type
     }
 deriving (Show)

class MkTypeDecl a where
  mkTypeDecl :: Name -> Location -> Type -> a

ppTypeDeclaration :: TypeDeclaration -> Doc a
ppTypeDeclaration td =
  ppName (_tdName td) <+> text "::" <+> ppType (_tdType td)

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
  mkValueDecl :: Name -> Location -> Expression -> a

ppValueDeclaration :: ValueDeclaration -> Doc a
ppValueDeclaration vd = typedecl $+$ valuedecl
 where
  typedecl
    | Just dt <- _vdDeclaredType vd =
        ppTypeDeclaration (TypeDeclaration (_vdName vd) (_vdLocation vd) dt)
    | otherwise = empty
  valuedecl = ppName (_vdName vd) <+> text "=" <+> ppExpression (_vdValue vd)

instance MkValueDecl ValueDeclaration where
  mkValueDecl n l e = ValueDeclaration n l [] [] Nothing e

instance MkValueDecl Declaration where
  mkValueDecl n l e = DeclVal (ValueDeclaration n l [] [] Nothing e)

-- -----------------------------------------------------------------------------

data PrimitiveDeclaration = PrimitiveDeclaration
     { _pdName     :: Name
     , _pdLocation :: Location
     , _pdLLVMType :: Text
     }
 deriving (Show)

class MkPrimDecl a where
  mkPrimDecl :: Name -> Location -> Text -> a

ppPrimitiveDeclaration :: PrimitiveDeclaration -> Doc a
ppPrimitiveDeclaration pd =
  text "primitive" <+> text "type" <+> ppName (_pdName pd) <+>
  text "=" <+> text' (_pdLLVMType pd)

instance MkPrimDecl PrimitiveDeclaration where
  mkPrimDecl = PrimitiveDeclaration

instance MkPrimDecl Declaration where
  mkPrimDecl n l t = DeclPrim (PrimitiveDeclaration n l t)

-- -----------------------------------------------------------------------------

data Declaration = DeclType TypeDeclaration
                 | DeclVal  ValueDeclaration
                 | DeclPrim PrimitiveDeclaration
 deriving (Show)

ppDeclaration :: Declaration -> Doc a
ppDeclaration (DeclType d) = ppTypeDeclaration d
ppDeclaration (DeclVal  d) = ppValueDeclaration d
ppDeclaration (DeclPrim d) = ppPrimitiveDeclaration d

makeLenses ''TypeDeclaration
makeLenses ''ValueDeclaration
makeLenses ''PrimitiveDeclaration

declName :: Lens' Declaration Name
declName = lens getter setter
 where
  getter (DeclType d) = view tdName d
  getter (DeclVal  d) = view vdName d
  getter (DeclPrim d) = view pdName d
  setter (DeclType d) x = DeclType (set tdName x d)
  setter (DeclVal  d) x = DeclVal  (set vdName x d)
  setter (DeclPrim d) x = DeclPrim (set pdName x d)

