{-# LANGUAGE TemplateHaskell #-}
module Bang.Syntax.Location(
         Position, posRow, posColumn, posOffset
       , ppPosition
       , initialPosition
       , advanceWith, advanceWith'
       , Origin(..)
       , ppOrigin
       , Location(Location)
       , locSource, locStart, locEnd
       , ppLocation
       , Located(..)
       , locatedAt
       , unknownLocation
       )
 where

import           Bang.Utils.Pretty(BangDoc, word)
import           Control.Lens
import           Control.Lens.TH(makeLenses)
import           Data.Monoid((<>))
import           Data.Text.Lazy(Text)
import qualified Data.Text.Lazy as T
import           Text.PrettyPrint.Annotated(colon, parens, text)

data Position = Position {
       _posRow    :: Word
     , _posColumn :: Word
     , _posOffset :: Word
     }
 deriving (Show)

makeLenses ''Position

ppPosition :: Position -> BangDoc
ppPosition (Position r c _) = word r <> colon <> word c

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

data Origin = Unknown
            | Interactive
            | File FilePath
 deriving (Eq, Show)

ppOrigin :: Origin -> BangDoc
ppOrigin x =
  case x of
    Unknown     -> text "<unknown>"
    Interactive -> text "<interactive>"
    File f      -> text f

data Location = Location {
       _locSource :: Origin
     , _locStart  :: Position
     , _locEnd    :: Position
     }
 deriving (Eq, Show)

makeLenses ''Location

ppLocation :: Location -> BangDoc
ppLocation loc
  | start == end = ppOrigin src <> colon <> ppPosition start
  | view posRow start == view posRow end =
      ppOrigin src <> colon <> word (view posRow start) <> colon <>
      word (view posColumn start) <> text "–" <> word (view posColumn end)
  | otherwise =
      ppOrigin src <> colon <> parens (ppPosition start) <> text "–" <>
      parens (ppPosition end)
 where
  src   = view locSource loc
  start = view locStart  loc
  end   = view locEnd    loc

data Located a = Located !Location a

instance Show a => Show (Located a) where
  show (Located l x) = show x ++ " `locatedAt` " ++ show l

locatedAt :: a -> Location -> Located a
locatedAt a p = Located p a

unknownLocation :: Location
unknownLocation = Location Unknown initialPosition initialPosition
