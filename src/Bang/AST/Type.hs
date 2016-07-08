{-# LANGUAGE TemplateHaskell #-}
module Bang.AST.Type
       ( Type(..)
       , ppType
       , Kind(..)
       , ppKind
       , Kinded(..)
         -- * the unit time
       , UnitType
       , ppUnitType
         -- * primitive types
       , PrimitiveType
       , ppPrimitiveType
       , mkPrimType
       , ptLocation, ptName
         -- * reference types
       , ReferenceType
       , ppReferenceType
       , mkTypeRef
       , rtLocation, rtKind, rtName
         -- * lambda types
       , FunctionType
       , ppFunctionType
       , mkFunType
       , ftLocation, ftKind, ftArgumentTypes, ftResultType
         -- * type application
       , TypeApplication
       , ppTypeApplication
       , mkTypeApp
       , taLocation, taKind, taLeftType, taRightType
       )
 where

import Bang.AST.Name(Name, ppName)
import Bang.Syntax.Location(Location)
import Control.Lens.TH(makeLenses)
import Text.PrettyPrint.Annotated(Doc, (<+>), (<>), text, hsep)

data Kind = Star
          | Unknown
          | KindArrow Kind Kind
  deriving (Show, Eq)

ppKind :: Kind -> Doc a
ppKind Star            = text "*"
ppKind Unknown         = text "?"
ppKind (KindArrow a b) = ppKind a <+> text "->" <+> ppKind b

class Kinded a where
  kind :: a -> Kind

-- -----------------------------------------------------------------------------

data UnitType = UnitType
 deriving (Show)

instance Kinded UnitType where
  kind _ = Star

ppUnitType :: UnitType -> Doc a
ppUnitType _ = text "()"

-- -----------------------------------------------------------------------------

data PrimitiveType = PrimitiveType
     { _ptLocation :: Location
     , _ptName     :: Name
     }
 deriving (Show)

class MkPrimType a where
  mkPrimType :: Location -> Name -> a

instance Kinded PrimitiveType where
  kind _ = Star

instance MkPrimType PrimitiveType where
  mkPrimType = PrimitiveType

instance MkPrimType Type where
  mkPrimType l n = TypePrim (PrimitiveType l n)

ppPrimitiveType :: PrimitiveType -> Doc a
ppPrimitiveType pt = text "llvm:" <> ppName (_ptName pt)

-- -----------------------------------------------------------------------------

data ReferenceType = ReferenceType
     { _rtLocation :: Location
     , _rtKind     :: Kind
     , _rtName     :: Name
     }
 deriving (Show)

instance Kinded ReferenceType where
  kind = _rtKind

ppReferenceType :: ReferenceType -> Doc a
ppReferenceType = ppName . _rtName

class MkTypeRef a where
  mkTypeRef :: Location -> Kind -> Name -> a

instance MkTypeRef ReferenceType where
  mkTypeRef = ReferenceType

instance MkTypeRef Type where
  mkTypeRef l k n = TypeRef (ReferenceType l k n)

-- -----------------------------------------------------------------------------

data FunctionType = FunctionType
     { _ftLocation      :: Location
     , _ftKind          :: Kind
     , _ftArgumentTypes :: [Type]
     , _ftResultType    :: Type
     }
 deriving (Show)

class MkFunType a where
  mkFunType :: Location -> [Type] -> Type -> a

instance MkFunType FunctionType where
  mkFunType l a r = FunctionType l Star a r

instance MkFunType Type where
  mkFunType l a r = TypeFun (FunctionType l Star a r)

ppFunctionType :: FunctionType -> Doc a
ppFunctionType ft =
  hsep (map ppType (_ftArgumentTypes ft)) <+> text "->" <+>
  ppType (_ftResultType ft)

instance Kinded FunctionType where
  kind = _ftKind

-- -----------------------------------------------------------------------------

data TypeApplication = TypeApplication
     { _taLocation  :: Location
     , _taKind      :: Kind
     , _taLeftType  :: Type
     , _taRightType :: Type
     }
 deriving (Show)

class MkTypeApp a where
  mkTypeApp :: Location -> Type -> Type -> a

instance MkTypeApp TypeApplication where
  mkTypeApp l s t  = TypeApplication l Unknown s t

instance MkTypeApp Type where
  mkTypeApp l s t = TypeApp (TypeApplication l Unknown s t)

instance Kinded TypeApplication where
  kind = _taKind

ppTypeApplication :: TypeApplication -> Doc a
ppTypeApplication ta =
  ppType (_taLeftType ta) <+> ppType (_taRightType ta)

-- -----------------------------------------------------------------------------

data Type = TypeUnit UnitType
          | TypePrim PrimitiveType
          | TypeRef  ReferenceType
          | TypeFun  FunctionType
          | TypeApp  TypeApplication
 deriving (Show)

ppType :: Type -> Doc a
ppType (TypeUnit t) = ppUnitType        t
ppType (TypePrim t) = ppPrimitiveType   t
ppType (TypeRef  t) = ppReferenceType   t
ppType (TypeFun  t) = ppFunctionType    t
ppType (TypeApp  t) = ppTypeApplication t

instance Kinded Type where
  kind (TypeUnit x) = kind x
  kind (TypePrim x) = kind x
  kind (TypeRef  x) = kind x
  kind (TypeFun  x) = kind x
  kind (TypeApp  x) = kind x

makeLenses ''PrimitiveType
makeLenses ''ReferenceType
makeLenses ''FunctionType
makeLenses ''TypeApplication
