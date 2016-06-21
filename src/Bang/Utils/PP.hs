module Bang.Utils.PP(
         PP(..)
       )
 where

import 

class PP a where
  ppr :: a -> Doc
