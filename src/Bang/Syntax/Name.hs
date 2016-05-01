{-# LANGUAGE TemplateHaskell #-}
module Bang.Syntax.Name(
         Name
       , nameId
       , nameString
       , nameGenerated
       )
 where

import Control.Lens.TH(makeLenses)

data Name = Name {
       _nameId        :: !Word
     , _nameString    :: !String
     , _nameGenerated :: !Bool
     }

makeLenses ''Name

instance Eq Name where
  a == b = _nameId a == _nameId b
  a /= b = _nameId a /= _nameId b

instance Ord Name where
  compare a b = compare (_nameId a) (_nameId b)
  max     a b = if a > b then a else b
  min     a b = if a > b then b else a
  a <  b      = _nameId a <  _nameId b
  a <= b      = _nameId a <= _nameId b
  a >  b      = _nameId a >  _nameId b
  a >= b      = _nameId a >= _nameId b

