{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Bang.AST.Name(
         NameEnvironment(..)
       , Name
       , nothingName
       , mkName
       , ppName
       , nameText
       , nameEnvironment
       , nameLocation
       , nameIndex
       )
 where

import Control.Lens(view)
import Control.Lens.TH(makeLenses)
import Data.Text.Lazy(Text, unpack)
import Data.Word(Word)
import Bang.Syntax.Location(Location, fakeLocation)
import Bang.Utils.Pretty(text', word)
import Text.PrettyPrint.Annotated(Doc, colon, (<>))

data NameEnvironment = ModuleEnv | TypeEnv | VarEnv
 deriving (Eq, Ord, Show)

data Name = Name
     { _nameText        :: Text
     , _nameEnvironment :: NameEnvironment
     , _nameLocation    :: Location
     , _nameIndex       :: Word
     }

makeLenses ''Name

nothingName :: Name
nothingName = Name ":<nothing>:" VarEnv fakeLocation 0

mkName :: Text -> NameEnvironment -> Location -> Word -> Name
mkName = Name

ppName :: Name -> Doc a
ppName n = text' (view nameText n) <> colon <> word (view nameIndex n)

instance Eq Name where
  a == b = view nameIndex a == view nameIndex b
  a /= b = view nameIndex a /= view nameIndex b

instance Ord Name where
  compare a b = compare (view nameIndex a) (view nameIndex b)
  max     a b = if a < b then b else a
  min     a b = if a < b then a else b
  (<)     a b = (<)     (view nameIndex a) (view nameIndex b)
  (>)     a b = (>)     (view nameIndex a) (view nameIndex b)
  (<=)    a b = (<=)    (view nameIndex a) (view nameIndex b)
  (>=)    a b = (>=)    (view nameIndex a) (view nameIndex b)

instance Show Name where
  show n = unpack (view nameText n) ++ ":" ++ show (view nameIndex n)
