{-# LANGUAGE TemplateHaskell #-}
module Bang.AST.Expression
       ( Expression
       , ppExpression
       -- * Constant Expressions
       , ConstantExpression
       , ppConstantExpression
       , mkConstExp
       , constLocation
       , constValue
       , ConstantValue(..)
       , ppConstantValue
       -- * References
       , ReferenceExpression
       , ppReferenceExpression
       , mkRefExp
       , refLocation
       , refName
       -- * Lambdas
       , LambdaExpression
       , ppLambdaExpression
       , mkLambdaExp
       , lambdaLocation
       , lambdaArgumentNames
       , lambdaBody
       )
 where

import Bang.Syntax.Location(Location)
import Bang.AST.Name(Name, ppName)
import Bang.Utils.Pretty(text')
import Control.Lens.TH(makeLenses)
import Data.Text.Lazy(Text)
import Text.PrettyPrint.Annotated(Doc, text, hsep, (<>), (<+>))

-- -----------------------------------------------------------------------------

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
    ConstantInt    _  _ -> error "Internal error: bad base for constant"
    ConstantChar      c -> text' c
    ConstantString    s -> text' s
    ConstantFloat     f -> text' f

data ConstantExpression = ConstantExpression
     { _constLocation :: Location
     , _constValue    :: ConstantValue
     }
 deriving (Show)

class MkConstExp a where
  mkConstExp :: Location -> ConstantValue -> a

instance MkConstExp ConstantExpression where
  mkConstExp = ConstantExpression

instance MkConstExp Expression where
  mkConstExp l v = ConstExp (mkConstExp l v)

ppConstantExpression :: ConstantExpression -> Doc a
ppConstantExpression = ppConstantValue . _constValue

-- -----------------------------------------------------------------------------

data ReferenceExpression = ReferenceExpression
     { _refLocation :: Location
     , _refName     :: Name
     }
 deriving (Show)

ppReferenceExpression :: ReferenceExpression -> Doc a
ppReferenceExpression = ppName . _refName

class MkRefExp a where
  mkRefExp :: Location -> Name -> a

instance MkRefExp ReferenceExpression where
  mkRefExp = ReferenceExpression

instance MkRefExp Expression where
  mkRefExp l n = RefExp (ReferenceExpression l n)

-- -----------------------------------------------------------------------------

data LambdaExpression = LambdaExpression
     { _lambdaLocation      :: Location
     , _lambdaArgumentNames :: [Name]
     , _lambdaBody          :: Expression
     }
 deriving (Show)

class MkLambdaExp a where
  mkLambdaExp :: Location -> [Name] -> Expression -> a

ppLambdaExpression :: LambdaExpression -> Doc a
ppLambdaExpression le =
  text "λ" <+> hsep (map ppName (_lambdaArgumentNames le)) <+> text "->" <+>
  ppExpression (_lambdaBody le)

instance MkLambdaExp LambdaExpression where
  mkLambdaExp = LambdaExpression

instance MkLambdaExp Expression where
  mkLambdaExp l a b = LambdaExp (LambdaExpression l a b)

-- -----------------------------------------------------------------------------

data Expression = ConstExp  ConstantExpression
                | RefExp    ReferenceExpression
                | LambdaExp LambdaExpression
 deriving (Show)

ppExpression :: Expression -> Doc a
ppExpression (ConstExp  e) = ppConstantExpression  e
ppExpression (RefExp    e) = ppReferenceExpression e
ppExpression (LambdaExp e) = ppLambdaExpression    e

makeLenses ''ConstantExpression
makeLenses ''ReferenceExpression
makeLenses ''LambdaExpression
