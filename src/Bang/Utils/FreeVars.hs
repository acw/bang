module Bang.Utils.FreeVars(
         CanHaveFreeVars(..)
       )
 where

import           Bang.AST.Name(Name)
import           Data.Set(Set)
import qualified Data.Set as Set

class CanHaveFreeVars a where
  freeVariables :: a -> Set Name

instance CanHaveFreeVars a => CanHaveFreeVars (Maybe a) where
  freeVariables (Just x) = freeVariables x
  freeVariables Nothing  = Set.empty

instance CanHaveFreeVars a => CanHaveFreeVars [a] where
  freeVariables []     = Set.empty
  freeVariables (x:xs) = freeVariables x `Set.union` freeVariables xs

