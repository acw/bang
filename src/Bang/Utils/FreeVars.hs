module Bang.Utils.FreeVars(
         CanHaveFreeVars(..)
       )
 where

import Bang.AST.Name(Name)

class CanHaveFreeVars a where
  freeVariables :: a -> [Name]

instance CanHaveFreeVars a => CanHaveFreeVars (Maybe a) where
  freeVariables (Just x) = freeVariables x
  freeVariables Nothing  = []
