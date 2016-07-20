module Bang.Syntax.PostProcess(
         runPostProcessor
       )
 where

import           Bang.AST(Name, Module, moduleDeclarations, ppName)
import           Bang.AST.Declaration(Declaration(..), declName,
                                      ValueDeclaration,
                                      vdName, vdLocation, vdDeclaredType,
                                      vdValue)
import           Bang.AST.Expression(isEmptyExpression)
import           Bang.AST.Type(Type)
import           Bang.Monad(Compiler, BangError(..), err)
import           Bang.Syntax.Location(Location, ppLocation)
import           Bang.Utils.FreeVars(CanHaveFreeVars(..))
import           Bang.Utils.Pretty(BangDoc)
import           Control.Lens(view, set)
import           Control.Monad(foldM)
import           Data.Graph(SCC(..))
import           Data.Graph.SCC(stronglyConnComp)
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
  do let decls = concat (view moduleDeclarations mdl)
     declTable <- makeDeclarationTable decls
     decls' <- combineTypeValueDeclarations declTable decls
     return (set moduleDeclarations (orderDecls decls') mdl)

-- -----------------------------------------------------------------------------

type DeclarationTable = Map Name (Maybe (Type, Location), Maybe ValueDeclaration)

makeDeclarationTable :: [Declaration] -> Compiler ps DeclarationTable
makeDeclarationTable decls = foldM combine Map.empty decls
 where
  combine table d =
    do let name = view declName d
       case d of
         DeclType _ ->
           return table
         DeclVal vd | Just t <- view vdDeclaredType vd,
                      isEmptyExpression (view vdValue vd) ->
           do let myLoc = view vdLocation vd
                  myVal = Just (t, myLoc)
              case Map.lookup name table of
                Nothing            ->
                  return (Map.insert name (myVal, Nothing) table)
                Just (Nothing, vd') ->
                  return (Map.insert name (myVal, vd') table)
                Just (Just (_, theirLoc), _) ->
                  err (RedefinitionError name myLoc theirLoc)
         DeclVal vd | Just _ <- view vdDeclaredType vd ->
           err (InternalError name)
         DeclVal vd | isEmptyExpression (view vdValue vd) ->
           err (InternalError name)
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

-- -----------------------------------------------------------------------------

combineTypeValueDeclarations :: DeclarationTable ->
                                [Declaration] ->
                                Compiler ps [Declaration]
combineTypeValueDeclarations table decls = process decls
 where
  process [] = return []
  process (x:rest) =
    case x of
      DeclType _ ->
        (x:) `fmap` process rest
      DeclVal vd | Just _ <- view vdDeclaredType vd,
                   isEmptyExpression (view vdValue vd) ->
        process rest
      DeclVal vd ->
        case Map.lookup (view vdName vd) table of
          Nothing ->
            err (InternalError (view vdName vd))
          Just (Nothing, _) ->
            (x:) `fmap` process rest
          Just (Just (t, _), _) ->
            do let vd' = set vdDeclaredType (Just t) vd
               (DeclVal vd' :) `fmap` process rest

-- -----------------------------------------------------------------------------

orderDecls :: [Declaration] -> [[Declaration]]
orderDecls decls = map unSCC (stronglyConnComp nodes)
 where
  unSCC (AcyclicSCC x) = [x]
  unSCC (CyclicSCC xs) = xs
  --
  nodes = map tuplify decls
  tuplify d = (d, view declName d, freeVariables d)
