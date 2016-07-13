module Bang.Syntax.PostProcess(
         runPostProcessor
       )
 where

import           Bang.AST(Name, Module, moduleDeclarations, ppName)
import           Bang.AST.Declaration(Declaration(..), declName,
                                      TypeDeclaration, ValueDeclaration,
                                      tdName, tdLocation, tdType,
                                      vdName, vdLocation, vdDeclaredType)
import           Bang.Monad(Compiler, BangError(..), err)
import           Bang.Syntax.Location(Location, ppLocation)
import           Bang.Utils.Pretty(BangDoc)
import           Control.Lens(view, set)
import           Control.Monad(foldM)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Text.PrettyPrint.Annotated(text, ($+$), (<+>), nest)

data PostProcessError = InternalError        Name
                      | RedefinitionError    Name Location Location
                      | TypeDeclWithoutValue Name Location
 deriving (Show)

instance BangError PostProcessError where
  ppError = prettyError

prettyError :: PostProcessError -> (Maybe Location, BangDoc)
prettyError e =
  case e of
    InternalError n ->
      (Nothing, text "Serious post-processing error w.r.t. " <+> ppName n)
    RedefinitionError n l1 l2 ->
      (Just l1, text "Name" <+> ppName n <+> text "redefined." $+$
                nest 2 (text "original definiton at " <+> ppLocation l2))
    TypeDeclWithoutValue n l ->
      (Just l, text "Type declaration provided, but no value provided." $+$
               nest 2 (text "variable name: " <+> ppName n))

runPostProcessor :: Module -> Compiler ps Module
runPostProcessor mdl =
  do declTable <- makeDeclarationTable mdl
     mdl' <- combineTypeValueDeclarations declTable mdl
     return mdl'

-- -----------------------------------------------------------------------------

type DeclarationTable = Map Name (Maybe TypeDeclaration, Maybe ValueDeclaration)

makeDeclarationTable :: Module -> Compiler ps DeclarationTable
makeDeclarationTable m = foldM combine Map.empty (view moduleDeclarations m)
 where
  combine table d =
    do let name = view declName d
       case d of
         DeclType td ->
           case Map.lookup name table of
             Nothing            ->
               return (Map.insert name (Just td, Nothing) table)
             Just (Nothing, vd) ->
               return (Map.insert name (Just td, vd) table)
             Just (Just td', _) ->
               do let newLoc  = view tdLocation td
                      origLoc = view tdLocation td'
                  err (RedefinitionError name newLoc origLoc)
         DeclVal vd ->
           case Map.lookup name table of
             Nothing            ->
               return (Map.insert name (Nothing, Just vd) table)
             Just (td, Nothing) ->
               return (Map.insert name (td, Just vd) table)
             Just (_, Just vd') ->
               do let newLoc  = view vdLocation vd
                      origLoc = view vdLocation vd'
                  err (RedefinitionError name newLoc origLoc)
         DeclPrim _ ->
           return table

-- -----------------------------------------------------------------------------

combineTypeValueDeclarations :: DeclarationTable -> Module -> Compiler ps Module
combineTypeValueDeclarations table m =
  do let decls = view moduleDeclarations m
     decls' <- process decls
     return (set moduleDeclarations decls' m)
 where
  process [] = return []
  process (x:rest) =
    case x of
      DeclPrim _  -> (x:) `fmap` process rest
      DeclType td ->
        case Map.lookup (view tdName td) table of
          Just (_, Nothing) ->
            err (TypeDeclWithoutValue (view tdName td) (view tdLocation td))
          _ ->
            process rest
      DeclVal vd ->
        case Map.lookup (view vdName vd) table of
          Nothing ->
            err (InternalError (view vdName vd))
          Just (Nothing, _) ->
            (x:) `fmap` process rest
          Just (Just td, _) ->
            do let vd' = set vdDeclaredType (Just (view tdType td)) vd
               (DeclVal vd' :) `fmap` process rest 
