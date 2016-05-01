{-# LANGUAGE TemplateHaskell #-}
module Bang.Syntax.Location(
         Position, posRow, posColumn, posOffset
       , initialPosition
       , advanceWith, advanceWith'
       , showPosition
       , Origin(..)
       , Location(Location)
       , locSource, locStart, locEnd
       , Located(..)
       , locatedAt
       )
 where

import           Control.Lens.TH(makeLenses)
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T

data Position = Position {
       _posRow    :: Word
     , _posColumn :: Word
     , _posOffset :: Word
     }
 deriving (Show)

makeLenses ''Position

initialPosition :: Position
initialPosition = Position 1 1 0

instance Eq Position where
  a == b = _posOffset a == _posOffset b

advanceWith :: Position -> Char -> Position
advanceWith (Position r c o) '\t' = Position r     (c+8) (o+1)
advanceWith (Position r _ o) '\n' = Position (r+1) 1     (o+1)
advanceWith (Position r c o) _    = Position r     (c+1) (o+1)

advanceWith' :: Position -> Text -> Position
advanceWith' pos txt =
  case T.uncons txt of
    Nothing        -> pos
    Just (c, rest) -> advanceWith' (pos `advanceWith` c) rest

showPosition :: Position -> String
showPosition (Position r c _) = show r ++ ":" ++ show c

data Origin = Unknown
            | Interactive
            | File FilePath
 deriving (Eq, Show)

data Location = Location {
       _locSource :: Origin
     , _locStart  :: Position
     , _locEnd    :: Position
     }
 deriving (Eq, Show)

makeLenses ''Location

data Located a = Located !Location a

instance Show a => Show (Located a) where
  show (Located l x) = show x ++ " `locatedAt` " ++ show l

locatedAt :: a -> Location -> Located a
locatedAt a p = Located p a
