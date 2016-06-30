module Bang.Utils.Pretty(
         BangDoc
       , Annotation(..)
       , text'
       , word
       )
 where

import Data.Text.Lazy(Text, unpack)
import Text.PrettyPrint.Annotated(Doc, text, integer)

type BangDoc = Doc Annotation

data Annotation = KeywordAnnotation

text' :: Text -> Doc a
text' = text . unpack

word :: Word -> Doc a
word = integer . fromIntegral
