{-# LANGUAGE TemplateHaskell #-}
module Bang.AST
       ( Module
       , ppModule
       , mkModule
       , moduleName, moduleDeclarations
       , module Bang.AST.Declaration
       , module Bang.AST.Expression
       , module Bang.AST.Name
       , module Bang.AST.Type
       )
 where

import Bang.AST.Declaration
import Bang.AST.Expression
import Bang.AST.Name
import Bang.AST.Type
import Control.Lens(view)
import Control.Lens.TH(makeLenses)
import Text.PrettyPrint.Annotated(Doc, empty, text, (<+>), ($+$))

data Module = Module {
       _moduleName         :: Name
     , _moduleDeclarations :: [Declaration]
     }

mkModule :: Name -> [Declaration] -> Module
mkModule = Module

makeLenses ''Module

ppModule :: Module -> Doc a
ppModule m = text "module" <+> ppName (view moduleName m) $+$
             dump (view moduleName m) (view moduleDeclarations m)
 where
  dump _    [] = empty
  dump prev (x:rest)
    | prev == view declName x =
         ppDeclaration x $+$ dump prev rest
    | otherwise = 
         text "" $+$ dump (view declName x) (x:rest)

