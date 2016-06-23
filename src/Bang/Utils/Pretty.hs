module Bang.Utils.Pretty(
         BangDoc
       , Annotation(..)
       )
 where

import Text.PrettyPrint.Annotated(Doc)

type BangDoc = Doc Annotation

data Annotation = KeywordAnnotation

